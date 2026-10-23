package blockbox

import org.lwjgl.glfw.GLFW.*

object TextEditingTest:
  def run(): Unit =
    var clipboard = ""
    val line = TextLine(64)
    def key(k: Int, mods: Int = 0): Unit =
      assert(line.key(k, mods, () => clipboard, value => clipboard = value))
    line.insert("hello world")
    key(GLFW_KEY_HOME); key(GLFW_KEY_RIGHT); line.insert("i")
    assert(line.text == "hiello world" && line.cursor == 2, "typing inserts at the caret")
    key(GLFW_KEY_DELETE); assert(line.text == "hillo world")
    key(GLFW_KEY_END)
    key(GLFW_KEY_BACKSPACE, GLFW_MOD_ALT)
    assert(line.text == "hillo ", "Alt+Backspace removes the preceding word")
    key(GLFW_KEY_BACKSPACE, GLFW_MOD_CONTROL)
    assert(line.text.isEmpty, "word deletion also skips intervening spaces")
    for _ <- 0 until 10 do key(GLFW_KEY_BACKSPACE)
    assert(line.text.isEmpty && line.cursor == 0, "repeated deletion stops at the boundary")
    line.insert("one two three")
    key(GLFW_KEY_LEFT, GLFW_MOD_CONTROL | GLFW_MOD_SHIFT)
    assert(line.selected == "three")
    key(GLFW_KEY_C, GLFW_MOD_CONTROL); assert(clipboard == "three")
    key(GLFW_KEY_X, GLFW_MOD_CONTROL); assert(line.text == "one two ")
    key(GLFW_KEY_Z, GLFW_MOD_CONTROL); assert(line.text == "one two three")
    key(GLFW_KEY_Z, GLFW_MOD_CONTROL | GLFW_MOD_SHIFT); assert(line.text == "one two ")
    clipboard = "four\nfive\t\u0000six"
    key(GLFW_KEY_V, GLFW_MOD_CONTROL)
    assert(line.text == "one two four five six", "paste stays on one line and strips controls")
    key(GLFW_KEY_A, GLFW_MOD_SUPER)
    clipboard = "\u0000\u001b"
    key(GLFW_KEY_V, GLFW_MOD_CONTROL)
    assert(line.text == "one two four five six", "unsupported paste cannot erase a selection")
    key(GLFW_KEY_BACKSPACE); assert(line.text.isEmpty, "Backspace deletes selected text")
    line.insert("abcdef")
    key(GLFW_KEY_HOME); key(GLFW_KEY_RIGHT, GLFW_MOD_SHIFT); key(GLFW_KEY_RIGHT, GLFW_MOD_SHIFT)
    key(GLFW_KEY_LEFT)
    assert(line.cursor == 0 && !line.hasSelection, "an arrow collapses selection")
    key(GLFW_KEY_DELETE, GLFW_MOD_CONTROL); assert(line.text.isEmpty)
    val capped = TextLine(5); capped.insert("abcde"); capped.move(1); capped.move(3, true)
    capped.insert("XYZ123");
    assert(capped.text == "aXYde", "paste truncates without losing the suffix")
    val name = TextLine(16, TextLine.playerName)
    name.insert("Alice bad|name\n0123456789")
    assert(name.text == "Alicebadname0123", "pasted names retain validation and length limits")
    line.synchronize("0123456789")
    assert(line.visible(4) == "789" && line.viewStart == 7)
    line.move(1); assert(line.visible(4) == "1234" && line.viewStart == 1)
    line.synchronize("history replacement"); key(GLFW_KEY_Z, GLFW_MOD_CONTROL)
    assert(line.text == "history replacement", "external history resets stale undo state")
    assert(!line.key(GLFW_KEY_ENTER, 0, () => clipboard, _ => ()))
    assert(TextLine.repeatable(GLFW_KEY_BACKSPACE) && TextLine.repeatable(GLFW_KEY_DELETE))
    fields()
    println(
      "[text-editing] insertion, selection, clipboard, word edits, repeat, undo, scroll and all field routes passed"
    )

  private def fields(): Unit =
    import InteractionSystemsTest.{call, field, set}
    val game = Blockbox()
    set(game, "screen", Screen.CreateWorld); set(game, "worldNameInput", "")
    val seed = field[Long](game, "worldSeed")
    val mode = field[GameMode](game, "createWorldMode")
    for c <- "Ranch MC Hide" do
      call(game, "onKey", Int.box(c.toUpper.toInt), Int.box(0))
      call(game, "onChar", Int.box(c.toInt))
    assert(field[String](game, "worldNameInput") == "Ranch MC Hide")
    assert(
      field[Long](game, "worldSeed") == seed && field[GameMode](game, "createWorldMode") == mode,
      "typing menu shortcut letters cannot change world settings"
    )
    call(game, "handleTextKey", Int.box(GLFW_KEY_BACKSPACE), Int.box(GLFW_MOD_ALT))
    assert(field[String](game, "worldNameInput") == "Ranch MC ")
    set(game, "createWorldNameFocused", false)
    call(game, "onChar", Int.box('4'.toInt))
    assert(field[String](game, "customSeedInput") == "4")
    assert(call(game, "chooseCreateSeed") == 4L, "the typed seed is the seed actually used")
    set(game, "customSeedInput", "0")
    assert(call(game, "chooseCreateSeed") == 0L, "zero is also a reproducible numeric seed")
    call(game, "previewRandomSeed")
    assert(
      call(game, "chooseCreateSeed").toString == field[String](game, "customSeedInput"),
      "random seed previews remain copyable and match the generated world"
    )
    set(game, "screen", Screen.JoinGame); set(game, "joinNameFocused", false)
    set(game, "joinIpInput", "server:25565")
    call(game, "handleTextKey", Int.box(GLFW_KEY_HOME), Int.box(0))
    call(game, "onChar", Int.box('a'.toInt))
    assert(field[String](game, "joinIpInput") == "aserver:25565")
    set(game, "joinNameFocused", true); set(game, "playerName", "Alex")
    call(game, "handleTextKey", Int.box(GLFW_KEY_BACKSPACE), Int.box(0))
    assert(field[String](game, "playerName") == "Ale")
    set(game, "screen", Screen.HostGame)
    call(game, "onChar", Int.box('x'.toInt)); assert(field[String](game, "playerName") == "Alex")
    set(game, "screen", Screen.Catalog); set(game, "catalogSearchFocused", true)
    set(game, "catalogQuery", "stone"); set(game, "catalogScroll", 9)
    call(game, "handleTextKey", Int.box(GLFW_KEY_BACKSPACE), Int.box(GLFW_MOD_CONTROL))
    assert(field[String](game, "catalogQuery").isEmpty && field[Int](game, "catalogScroll") == 0)
    set(game, "screen", Screen.Playing); set(game, "chatOpen", true)
    set(game, "chatInput", "/time day"); set(game, "chatHistoryIndex", 2)
    call(game, "handleTextKey", Int.box(GLFW_KEY_BACKSPACE), Int.box(GLFW_MOD_ALT))
    assert(
      field[String](game, "chatInput") == "/time " && field[Int](game, "chatHistoryIndex") == -1
    )

/** Uses the real desktop clipboard without starting the game or creating an OpenGL context. */
@main def runTextClipboardTests(): Unit =
  import InteractionSystemsTest.{call, field, set}
  glfwInitHint(GLFW_PLATFORM, GLFW_PLATFORM_X11)
  assert(glfwInit(), "GLFW clipboard test requires a display")
  glfwWindowHint(GLFW_VISIBLE, GLFW_FALSE); glfwWindowHint(GLFW_CLIENT_API, GLFW_NO_API)
  val window = glfwCreateWindow(160, 80, "Blockbox clipboard test", 0L, 0L)
  assert(window != 0L)
  val saved = Option(glfwGetClipboardString(window)).getOrElse("")
  try
    val game = Blockbox(); set(game, "window", window)
    set(game, "screen", Screen.JoinGame); set(game, "joinNameFocused", false)
    set(game, "joinIpInput", "localhost:25565")
    def key(k: Int, mods: Int = GLFW_MOD_CONTROL): Unit =
      call(game, "onKey", Int.box(k), Int.box(mods))
    key(GLFW_KEY_A); key(GLFW_KEY_C)
    assert(glfwGetClipboardString(window) == "localhost:25565")
    glfwSetClipboardString(window, "127.0.0.1:25566")
    key(GLFW_KEY_V)
    assert(field[String](game, "joinIpInput") == "127.0.0.1:25566")
    key(GLFW_KEY_A); key(GLFW_KEY_X)
    assert(field[String](game, "joinIpInput").isEmpty)
    key(GLFW_KEY_V); key(GLFW_KEY_BACKSPACE, GLFW_MOD_ALT)
    assert(field[String](game, "joinIpInput") == "127.0.0.1:")
    for _ <- 0 until 5 do key(GLFW_KEY_BACKSPACE, 0)
    assert(field[String](game, "joinIpInput") == "127.0")
    println("[text-clipboard] native copy, paste, cut, Alt+Backspace and repeated deletion passed")
  finally
    glfwSetClipboardString(window, saved)
    glfwDestroyWindow(window); glfwTerminate()
