package mhtml.todo

import scala.scalajs.js.JSApp

import mhtml.cats._
import mhtml._
import org.scalajs.dom._
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.{Event, KeyboardEvent, window}
import org.scalajs.dom.ext.{KeyCode, LocalStorage}
import org.scalajs.dom.raw.HTMLInputElement
import xml.Node
import upickle.default.{read, write}

object Todo extends JSApp {
  class Todo(val title: String, val completed: Boolean)
  object Todo {
    def apply(title: String, completed: Boolean) = new Todo(title, completed)
    def unapply(todo: Todo) = Option((todo.title, todo.completed))
  }

  case class TodoList(text: String, hash: String, items: Rx[Seq[Todo]])

  object Models {
    val LocalStorageName = "todo.mhtml"
    def load(): Seq[Todo] =
      LocalStorage(LocalStorageName).toSeq.flatMap(read[Seq[Todo]])
    def save(todos: Seq[Todo]): Unit =
      LocalStorage(LocalStorageName) = write(todos)
    val allTodos = Var[Seq[Todo]](load())
    val autoSave: Rx[Unit] = allTodos.map(save)
    val editingTodo = Var[Option[Todo]](None)
    val all = TodoList("All", "#/", allTodos)
    val active =
      TodoList("Active", "#/active", allTodos.map(_.filter(!_.completed)))
    val completed =
      TodoList("Completed", "#/completed", allTodos.map(_.filter(_.completed)))
    val todoLists = Seq(all, active, completed)
    def getCurrentTodoList: TodoList =
      todoLists.find(_.hash == window.location.hash).getOrElse(all)
    val currentTodoList = Var(getCurrentTodoList)
    window.onhashchange = { _: Event =>
      currentTodoList := getCurrentTodoList
    }
  }
  import Models._

  def header: Rx[xml.Node] = allTodos.map { todos =>
    val keyDownHandler = { event: KeyboardEvent =>
      (event.currentTarget, event.keyCode) match {
        case (input: HTMLInputElement, KeyCode.Enter) =>
          input.value.trim match {
            case "" =>
            case title =>
              allTodos := (Todo(title, completed = false) +: todos)
              input.value = ""
          }
        case _ =>
      }
    }
    <header class="header">
      <h1>todos</h1>
      <input class="new-todo"
             autofocus="true"
             placeholder="What needs to be done?"
             onkeydown={e: KeyboardEvent => keyDownHandler(e)}/>
    </header>
  }

  def todoListItem(todo: Todo): Rx[xml.Node] =
    for {
      todos <- allTodos
      editing <- editingTodo
    } yield {
      // onblur is not only triggered by user interaction, but also triggered by programmatic DOM changes.
      // In order to suppress this behavior, we have to replace the onblur event listener to a dummy handler before programmatic DOM changes.
      val suppressOnBlur = Var(false)
      def submit = { event: Event =>
        suppressOnBlur := true
        editingTodo := None
        event.currentTarget.asInstanceOf[HTMLInputElement].value.trim match {
          case "" =>
            allTodos := todos.filter(_ eq todo)
          case trimmedTitle =>
            allTodos := todos.map {
              case t if t eq todo =>
                Todo(trimmedTitle, todo.completed)
              case t => t
            }
        }
      }
      def keyDownHandler = { event: KeyboardEvent =>
        event.keyCode match {
          case KeyCode.Escape =>
            suppressOnBlur := true
            editingTodo := None
          case KeyCode.Enter =>
            submit(event)
          case _ =>
        }
      }
      def ignoreEvent = { _: Event =>
        }
      def blurHandler: Rx[Event => Any] =
        suppressOnBlur.map(x => if (x) ignoreEvent else submit)

      def toggleHandler: (Event) => Unit = { event: Event =>
        allTodos := todos.map {
          case t if t eq todo =>
            Todo(todo.title,
                 event.currentTarget.asInstanceOf[HTMLInputElement].checked)
          case t => t
        }
      }
      val onbldclick: (Event) => Unit = { _: Event =>
        editingTodo := Some(todo)
      }

      <li class={s"${if (todo.completed) "completed" else ""} ${if (editing.contains(todo)) "editing" else ""}"}>
      <div class="view">
        <input class="toggle" type="checkbox" checked={todo.completed.toString} onclick={toggleHandler}/>
        <label ondblclick={ _: Event =>  /*; editInput.focus() */ }>{ todo.title }</label>
        <button class="destroy" onclick={ _: Event => allTodos := todos.filterNot(_ eq todo) }></button>
      </div>
      <input id="editInput" class="edit" value={ todo.title } onblur={ ignoreEvent } onkeydown={ keyDownHandler } />
    </li>
    }
  implicit class SequencingListFFS[A](self: Seq[Rx[A]]) {
    def sequence: Rx[Seq[A]] =
      self.foldRight(Rx(List[A]()))(for { n <- _; s <- _ } yield n +: s)
  }

  def mainSection: Rx[xml.Node] =
    for {
      todos <- allTodos
      items <- active.items
      current <- currentTodoList
      currItems <- current.items
      lst <- currItems.map(todoListItem).sequence
    } yield {
      def toggleAllClickHandler = { event: Event =>
        for ((todo, i) <- todos.zipWithIndex) {
          if (todo.completed != event.currentTarget
                .asInstanceOf[HTMLInputElement]
                .checked) {
            allTodos := todos map {
              case t if t eq todo =>
                Todo(
                  todo.title,
                  event.currentTarget.asInstanceOf[HTMLInputElement].checked)
              case t => t

            }
          }
        }
      }
      <section class="main" style:display={if (todos.isEmpty) "none" else ""}>
      <input type="checkbox" class="toggle-all" checked={items.isEmpty.toString} onclick={toggleAllClickHandler}/>
      <label for="toggle-all">Mark all as complete</label>
      <ul class="todo-list">{lst}</ul>
    </section>
    }

  def footer: Rx[xml.Node] =
    for {
      todos <- allTodos
      items <- active.items
      completedItems <- completed.items
      current <- currentTodoList
    } yield {
      def clearCompletedClickHandler = { _: Event =>
        allTodos := todos.filter(_.completed)
      }
      <footer class="footer" style:display={if (todos.isEmpty) "none" else ""}>
      <span class="todo-count">
        <strong>{ items.length.toString }</strong> { if (items.length == 1) "item" else "items"} left
      </span>
      <ul class="filters">{
        todoLists.map { todoList =>
          <li>
            <a href={ todoList.hash } class={ if (todoList == current) "selected" else "" }>{ todoList.text }</a>
          </li>
        }
      }</ul>
      <button class="clear-completed" onclick={clearCompletedClickHandler}
              style:visibility={if (completedItems.isEmpty) "hidden" else "visible"}>
        Clear completed
      </button>
    </footer>
    }

  def todoapp: Rx[xml.Node] =
    for {
      h <- header
      mainS <- mainSection
      foot <- footer
    } yield {
      <div>
        <section class="todoapp">{ h }{ mainS }{ foot }</section>
        <footer class="info">
          <p>Double-click to edit a todo</p>
          <p>Written by <a href="https://github.com/atry">Yang Bo</a></p>
          <p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
        </footer>
      </div>
    }

  def main(): Unit = {
    val div = document.getElementById("application-container")
    mount(div, todoapp)
  }
}
