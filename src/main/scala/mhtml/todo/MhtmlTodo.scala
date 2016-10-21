// This implementation is mostly copied from Binding.scala TodoMVC example:
// https://github.com/ThoughtWorksInc/todo/blob/master/js/src/main/scala/com/thoughtworks/todo/Main.scala
package mhtml.todo
import scala.scalajs.js.JSApp
import scala.xml.Elem
import scala.xml.Node

import mhtml._
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.KeyboardEvent
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.ext.LocalStorage
import org.scalajs.dom.raw.HTMLInputElement
import upickle.default.read
import upickle.default.write

object MhtmlTodo extends JSApp {
  class Todo(val title: String, val completed: Boolean)
  object Todo {
    def apply(title: String, completed: Boolean) = new Todo(title, completed)
    def unapply(todo: Todo) = Option((todo.title, todo.completed))
  }

  case class TodoList(text: String, hash: String, items: Rx[Seq[Todo]])

  object Model {
    val LocalStorageName = "todo.mhtml"
    def load(): Seq[Todo] =
      LocalStorage(LocalStorageName).toSeq.flatMap(read[Seq[Todo]])
    def save(todos: Seq[Todo]): Unit =
      LocalStorage(LocalStorageName) = write(todos)
    val allTodos = Var[Seq[Todo]](load())
    val autoSave: Unit = allTodos.foreach(save)
    val editingTodo = Var[Option[Todo]](None)
    val all = TodoList("All", "#/", allTodos)
    val active =
      TodoList("Active", "#/active", allTodos.map(_.filter(!_.completed)))
    val completed =
      TodoList("Completed", "#/completed", allTodos.map(_.filter(_.completed)))
    val todoLists = Seq(all, active, completed)
    def getCurrentTodoList: TodoList =
      todoLists.find(_.hash == dom.window.location.hash).getOrElse(all)
    val currentTodoList = Var(getCurrentTodoList)
    dom.window.onhashchange = { _: Event =>
      currentTodoList := getCurrentTodoList
    }
  }

  // We're not strict about having all updates happen here, only the ones that
  // contain some non-trivial logic.
  object Update {
    import Model._
    def newTodo(title: String): Unit =
      allTodos.update(todos => Todo(title, completed = false) +: todos)
    def removeTodo(todo: Todo): Unit =
      allTodos.update(_.filterNot(_ eq todo))
    def updateTodo(toUpdate: Todo, newTodo: Todo) =
      allTodos.update(todos => todos.updated(todos.indexOf(toUpdate), newTodo))
    def setAllCompleted(completed: Boolean) =
      allTodos.update(_.map {
        case t if t.completed != completed =>
          Todo(t.title, completed)
        case t => t
      })
  }

  object View {
    import Model._
    import Update._
    // helper to render <input checked={null}> as <input>
    def conditionalAttribute(cond: Boolean) = if (cond) "true" else null

    val header: xml.Node = {
      val onInputKeydown: (KeyboardEvent) => Unit = { event: KeyboardEvent =>
        (event.currentTarget, event.keyCode) match {
          case (input: HTMLInputElement, KeyCode.Enter) =>
            input.value.trim match {
              case "" =>
              case title =>
                newTodo(title)
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
               onkeydown={onInputKeydown}/>
      </header>
    }

    def focusInput() = dom.document.getElementById("editInput") match {
      case t: HTMLInputElement => t.focus()
      case _ =>
    }

    def todoListItem(todo: Todo): Elem = {
      val suppressOnBlur = Var(false)
      def submit = { event: Event =>
        suppressOnBlur := true
        editingTodo := None
        event.currentTarget.asInstanceOf[HTMLInputElement].value.trim match {
          case "" =>
            removeTodo(todo)
          case trimmedTitle =>
            updateTodo(todo, Todo(trimmedTitle, todo.completed))
        }
      }
      def onEditTodoTitle = { event: KeyboardEvent =>
        event.keyCode match {
          case KeyCode.Escape =>
            suppressOnBlur := true
            editingTodo := None
          case KeyCode.Enter =>
            submit(event)
            focusInput()
          case _ =>
        }
      }
      def ignoreEvent: (Event) => Unit = _ => Unit
      def blurHandler: Rx[Event => Any] =
        suppressOnBlur.map(x => if (x) ignoreEvent else submit)
      def onToggleCompleted: (Event) => Unit = { event: Event =>
        event.currentTarget match {
          case input: HTMLInputElement =>
            updateTodo(todo, Todo(todo.title, input.checked))
          case _ =>
        }
      }
      val onDoubleClick: (Event) => Unit = { _: Event =>
        editingTodo := Some(todo)
        focusInput()
      }
      val onDelete: (Event) => Unit = _ => removeTodo(todo)

      val css = editingTodo.map { x =>
        val editing = if (x.contains(todo)) "editing" else ""
        val completed = if (todo.completed) "completed" else ""
        s"$editing $completed"
      }
      <li class={css}>
        <div class="view">
          <input onclick={onToggleCompleted}
                 class="toggle"
                 type="checkbox"
                 checked={conditionalAttribute(todo.completed)} />
          <label ondblclick={onDoubleClick}>{ todo.title }</label>
          <button onclick={onDelete} class="destroy"></button>
        </div>
        <input onkeydown={onEditTodoTitle}
               id="editInput"
               class="edit"
               value={todo.title}
               onblur={blurHandler}/>
      </li>
    }

    def toggleAllClickHandler = { event: Event =>
      event.currentTarget match {
        case input: HTMLInputElement =>
          setAllCompleted(input.checked)
        case _ =>
      }
    }

    def mainSection: Node = {
      val todoList: Rx[Seq[Elem]] = currentTodoList.flatMap { current =>
        current.items.map(_.map(todoListItem))
      }
      // TODO(olafur) This is broken in 0.1, fix here https://github.com/OlivierBlanvillain/monadic-html/pull/9
      val checked = active.items.map(x => conditionalAttribute(x.isEmpty))
      val display = allTodos.map(todos => if (todos.isEmpty) "none" else "")
      <section class="main" style:display={display}>
        <input onclick={toggleAllClickHandler}
               type="checkbox"
               class="toggle-all"
               checked={checked} />
        <label for="toggle-all" checked={checked}>Mark all as complete</label>
        <ul class="todo-list">{todoList}</ul>
      </section>
    }

    val count = active.items.map { items =>
      <span class="todo-count">
        <strong>{ items.length }</strong>
        {if (items.length == 1) "item" else "items"} left
      </span>
    }

    def todoListsFooter(todoList: TodoList) = {
      val css = currentTodoList.map(x => if (x == todoList) "selected" else "")
      <li>
        <a href={ todoList.hash } class={css}>{ todoList.text }</a>
      </li>
    }

    def footer: Node = {
      def onClearCompleted = { _: Event =>
        allTodos.update(_.filterNot(_.completed))
      }
      val display = allTodos.map(x => if (x.isEmpty) "none" else "")
      val visibility =
        completed.items.map(x => if (x.isEmpty) "hidden" else "visible")
      <footer class="footer" style:display={display}>
        <ul class="filters">{todoLists.map(todoListsFooter)}</ul>
        <button onclick={onClearCompleted}
                class="clear-completed"
                style:visibility={visibility}>
          Clear completed
        </button>
      </footer>
    }

    def todoapp: Node = {
      <div>
        <section class="todoapp">{header}{mainSection}{footer}</section>
        <footer class="info">
          <p>Double-click to edit a todo</p>
          <p>
            Originally written by <a href="https://github.com/atry">Yang Bo</a>,
            adapted to monadic-html by <a href="https://github.com/olafurpg">Olafur Pall Geirsson</a>.
          </p>
          <p>Part of <a href="http://todomvc.com">TodoMVC</a></p>
        </footer>
      </div>
    }
  }

  def main(): Unit = {
    val div = dom.document.getElementById("application-container")
    mount(div, View.todoapp)
  }
}
