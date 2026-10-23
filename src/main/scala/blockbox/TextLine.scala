package blockbox

import org.lwjgl.glfw.GLFW.*
import scala.collection.mutable

/** Single-line editing, independent of the window and clipboard implementation. */
final class TextLine(val limit: Int, accepts: Char => Boolean = TextLine.printable):
  require(limit > 0)
  private final case class State(text: String, cursor: Int, anchor: Int)
  private var state = State("", 0, 0)
  private val undoStack = mutable.ArrayDeque.empty[State]
  private val redoStack = mutable.ArrayDeque.empty[State]
  private var first = 0
  def text: String = state.text
  def cursor: Int = state.cursor
  def selectionStart: Int = cursor.min(state.anchor)
  def selectionEnd: Int = cursor.max(state.anchor)
  def selected: String = text.substring(selectionStart, selectionEnd)
  def hasSelection: Boolean = cursor != state.anchor
  def viewStart: Int = first

  /** History/completion and other external replacements start a fresh editing history. */
  def synchronize(value: String): Unit =
    val next = clean(value, limit)
    if next != text then
      state = State(next, next.length, next.length)
      first = 0; undoStack.clear(); redoStack.clear()

  private def clean(value: String, maximum: Int): String =
    Option(value)
      .getOrElse("")
      .iterator
      .map(c => if c == '\n' || c == '\r' || c == '\t' then ' ' else c)
      .filter(c => TextLine.printable(c) && accepts(c))
      .take(maximum.max(0))
      .mkString

  private def replace(from: Int, until: Int, value: String): Unit =
    val inserted = clean(value, limit - (text.length - (until - from)))
    val next = text.take(from) + inserted + text.drop(until)
    if next != text then
      undoStack.append(state)
      if undoStack.size > 64 then undoStack.removeHead()
      redoStack.clear()
    val end = from + inserted.length
    state = State(next, end, end)

  def insert(value: String): Unit =
    // Unsupported or empty clipboard text must not erase the current selection.
    if clean(value, limit).nonEmpty then replace(selectionStart, selectionEnd, value)

  def move(to: Int, selecting: Boolean = false): Unit =
    val next = to.max(0).min(text.length)
    state = state.copy(cursor = next, anchor = if selecting then state.anchor else next)

  private def category(c: Char): Int =
    if c.isWhitespace then 0 else if c.isLetterOrDigit || c == '_' then 1 else 2
  private def previousWord: Int =
    var i = cursor
    while i > 0 && text(i - 1).isWhitespace do i -= 1
    if i > 0 then
      val kind = category(text(i - 1))
      while i > 0 && category(text(i - 1)) == kind do i -= 1
    i
  private def nextWord: Int =
    var i = cursor
    if i < text.length then
      val kind = category(text(i))
      while i < text.length && category(text(i)) == kind do i += 1
    while i < text.length && text(i).isWhitespace do i += 1
    i

  /** Returns true for handled editing keys; Enter/Tab/Escape/history remain with the UI. */
  def key(
      key: Int,
      mods: Int,
      readClipboard: () => String,
      writeClipboard: String => Unit
  ): Boolean =
    val shift = (mods & GLFW_MOD_SHIFT) != 0
    val shortcut = (mods & (GLFW_MOD_CONTROL | GLFW_MOD_SUPER)) != 0
    val word = (mods & (GLFW_MOD_CONTROL | GLFW_MOD_ALT)) != 0
    if shortcut && key == GLFW_KEY_A then { move(0); move(text.length, selecting = true) }
    else if shortcut && key == GLFW_KEY_C then
      if hasSelection then writeClipboard(selected)
    else if shortcut && key == GLFW_KEY_X then
      if hasSelection then { writeClipboard(selected); replace(selectionStart, selectionEnd, "") }
    else if shortcut && key == GLFW_KEY_V then insert(readClipboard())
    else if shortcut && (key == GLFW_KEY_Z || key == GLFW_KEY_Y) then
      val redo = key == GLFW_KEY_Y || shift
      val from = if redo then redoStack else undoStack
      val to = if redo then undoStack else redoStack
      if from.nonEmpty then { to.append(state); state = from.removeLast() }
    else if key == GLFW_KEY_LEFT || key == GLFW_KEY_RIGHT then
      val left = key == GLFW_KEY_LEFT
      val target = if hasSelection && !shift then (if left then selectionStart else selectionEnd)
      else if word then (if left then previousWord else nextWord)
      else cursor + (if left then -1 else 1)
      move(target, shift)
    else if key == GLFW_KEY_HOME then move(0, shift)
    else if key == GLFW_KEY_END then move(text.length, shift)
    else if key == GLFW_KEY_BACKSPACE || key == GLFW_KEY_DELETE then
      if hasSelection then replace(selectionStart, selectionEnd, "")
      else if key == GLFW_KEY_BACKSPACE && cursor > 0 then
        replace(if word then previousWord else cursor - 1, cursor, "")
      else if key == GLFW_KEY_DELETE && cursor < text.length then
        replace(cursor, if word then nextWord else cursor + 1, "")
    else return false
    true

  /** Keep the caret in view without shrinking the font or losing the real character positions. */
  def visible(columns: Int): String =
    val count = columns.max(1)
    first = first.min((text.length - count + 1).max(0))
    if cursor < first then first = cursor
    if cursor >= first + count then first = cursor - count + 1
    text.slice(first, first + count)

object TextLine:
  def printable(c: Char): Boolean = c >= ' ' && c <= '~'
  def playerName(c: Char): Boolean = c.isLetterOrDigit || c == '_' || c == '-'
  def repeatable(key: Int): Boolean = Set(
    GLFW_KEY_BACKSPACE,
    GLFW_KEY_DELETE,
    GLFW_KEY_LEFT,
    GLFW_KEY_RIGHT,
    GLFW_KEY_HOME,
    GLFW_KEY_END
  ).contains(key)
