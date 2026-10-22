package blockbox

import groovy.lang.Closure
import java.util.function.Consumer
import scala.collection.mutable
import scala.math.*
import org.lwjgl.opengl.GL11.*

final class ModSubscription(private val cancel: () => Unit):
  def close(): Unit = cancel()
  def unsubscribe(): Unit = cancel()

trait ModCancellable:
  var cancelled: Boolean
  def isCancelled: Boolean = cancelled
  def getCancelled: Boolean = cancelled
  def setCancelled(value: Boolean): Unit = cancelled=value

final case class ModInteractionEvent(api: ModApi,playerName: String,x: Int,y: Int,z: Int,block: Block,var cancelled: Boolean=false,var handled: Boolean=false) extends ModCancellable:
  def getApi: ModApi=api
  def getPlayerName: String=playerName
  def getX: Int=x
  def getY: Int=y
  def getZ: Int=z
  def getBlock: Block=block
  def isHandled: Boolean=handled
  def setHandled(value: Boolean): Unit=handled=value

final case class ModDamageEvent(api: ModApi,playerName: String,reason: String,var amount: Float,var cancelled: Boolean=false) extends ModCancellable:
  def getApi: ModApi=api
  def getPlayerName: String=playerName
  def getReason: String=reason
  def getAmount: Float=amount
  def setAmount(value: Number): Unit=amount=Option(value).map(_.floatValue()).filter(_.isFinite).getOrElse(0f).max(0f).min(1000f)

final case class ModMoveEvent(api: ModApi,playerName: String,previous: Vec3,var destination: Vec3,dt: Float,var cancelled: Boolean=false) extends ModCancellable:
  def getApi: ModApi=api
  def getPlayerName: String=playerName
  def getPrevious: Vec3=previous
  def getDestination: Vec3=destination
  def setDestination(value: Vec3): Unit=if value!=null then destination=value
  def getDt: Float=dt

final case class ModPlayerEvent(api: ModApi,playerName: String,reason: String):
  def getApi: ModApi=api
  def getPlayerName: String=playerName
  def getReason: String=reason

final case class ModWorldRenderEvent(api: ModApi,render: ModRenderApi,dt: Float):
  def getApi: ModApi=api
  def getRender: ModRenderApi=render
  def getDt: Float=dt

final class ModGraphicsApi(private val game: Blockbox):
  def shaderPacks: java.util.List[String]=
    import scala.jdk.CollectionConverters.*
    game.modShaderPacks.asJava
  def getShaderPacks: java.util.List[String]=shaderPacks
  def selectShaderPack(name: String): Boolean=game.modSelectShaderPack(name)
  def uniform(name: String,value: Number): Unit=game.modShaderUniform(name,value.floatValue())

/** Finite bounded rules are shared by physics, interaction and the host clock. */
final class ModRulesApi(private val mayChange: () => Boolean=() => true,private val changed: () => Unit=() => ()):
  private val definitions=Map(
    "gravity" -> (52f,0f,200f), "jumpSpeed" -> (12.5f,0f,40f),
    "moveSpeed" -> (1f,0.1f,3f), "reach" -> (8f,1f,10f),
    "miningSpeed" -> (1f,0.05f,100f), "timeScale" -> (1f,0f,20f),
    "fallDamage" -> (1f,0f,10f), "regeneration" -> (1f,0f,10f),
    "weather" -> (-1f,-1f,1f))
  private val values=mutable.HashMap.from(definitions.map((key,value) => key -> value._1))
  def get(name: String): Float=values.getOrElse(name,throw IllegalArgumentException("Unknown game rule: "+name))
  def set(name: String,value: Number): Unit=
    require(mayChange(),"Only the host may change synchronized game rules")
    update(name,Option(value).map(_.floatValue()).getOrElse(Float.NaN)); changed()
  private def update(name: String,value: Float): Unit=
    val (_,low,high)=definitions.getOrElse(name,throw IllegalArgumentException("Unknown game rule: "+name))
    require(value.isFinite && value>=low && value<=high,s"$name must be finite and in $low..$high")
    values(name)=value
  def reset(): Unit=
    require(mayChange(),"Only the host may change rules")
    definitions.foreach((key,value) => values(key)=value._1); changed()
  def snapshot: String=values.toVector.sortBy(_._1).map((k,v) => s"$k=$v").mkString(",")
  private[blockbox] def receive(text: String): Unit=
    require(text.length<=1024,"Game rule snapshot too large")
    val parsed=text.split(',').map { entry =>
      val pair=entry.split('='); require(pair.length==2,"Invalid game rule")
      val value=pair(1).toFloat
      val (_,low,high)=definitions.getOrElse(pair(0),throw IllegalArgumentException("Unknown game rule"))
      require(value.isFinite && value>=low && value<=high,"Invalid game rule value")
      pair(0)->value
    }
    require(parsed.map(_._1).distinct.length==parsed.length,"Duplicate game rule")
    parsed.foreach((key,value) => values(key)=value)

/** Generation callbacks operate on a private chunk transaction, never a live world. */
final class ModChunkEvent(val seed: Long,val chunkX: Int,val chunkZ: Int,private val data: Array[Byte]):
  val random=new java.util.Random(seed^(chunkX.toLong*341873128712L)^(chunkZ.toLong*132897987541L))
  def getSeed: Long=seed
  def getChunkX: Int=chunkX
  def getChunkZ: Int=chunkZ
  def getRandom: java.util.Random=random
  def worldX(x: Int): Int=chunkX*16+x
  def worldZ(z: Int): Int=chunkZ*16+z
  private def index(x: Int,y: Int,z: Int): Int=
    require(x>=0 && x<16 && z>=0 && z<16 && y>=0 && y<Terrain.worldHeight,"Chunk coordinates outside 16 x 128 x 16")
    (y*16+z)*16+x
  def get(x: Int,y: Int,z: Int): Block=Block.fromId(data(index(x,y,z)))
  def set(x: Int,y: Int,z: Int,block: Block): Unit=
    require(block!=null && CustomBlocks.registered(block),"Unknown block")
    data(index(x,y,z))=block.id
  def height(x: Int,z: Int): Int=
    var y=Terrain.worldHeight-1
    while y>0 && get(x,y,z)==Block.Air do y-=1
    y
  def replace(from: Block,to: Block): Unit=
    require(to!=null && CustomBlocks.registered(to),"Unknown replacement")
    var i=0
    while i<data.length do
      if data(i)==from.id then data(i)=to.id
      i+=1
  def fill(x0: Int,y0: Int,z0: Int,x1: Int,y1: Int,z1: Int,block: Block): Unit=
    index(x0,y0,z0); index(x1,y1,z1)
    for x <- x0.min(x1) to x0.max(x1); y <- y0.min(y1) to y0.max(y1); z <- z0.min(z1) to z0.max(z1) do set(x,y,z,block)

final class ModWorldgenApi:
  private final class Generator(val handler: Consumer[ModChunkEvent]):
    var failures=0
    var disabled=false
  @volatile private var generators=Vector.empty[Generator]
  @volatile private var sealedGenerators=false
  private[blockbox] def checkpoint(): () => Unit=
    val saved=generators
    () => {generators=saved}
  def onChunk(handler: Consumer[ModChunkEvent]): Unit=synchronized {
    require(!sealedGenerators,"Register world generators during onInit")
    require(handler!=null && generators.length<32,"World generator limit is 32")
    generators=generators:+Generator(handler)
  }
  def onChunk(handler: Closure[?]): Unit=onChunk(ModRuntime.consumer[ModChunkEvent](handler))
  private[blockbox] def decorate(seed: Long,x: Int,z: Int,data: Array[Byte]): Unit=
    sealedGenerators=true
    generators.foreach { g => g.synchronized {
      if !g.disabled then
        val working=data.clone()
        try
          g.handler.accept(ModChunkEvent(seed,x,z,working))
          System.arraycopy(working,0,data,0,data.length)
        catch
          case e: Exception =>
            g.failures+=1; g.disabled=g.failures>=3
            System.err.println(s"Mod world generator failed (${g.failures}/3); chunk changes rolled back: ${e.getMessage}")
    }}

/** Helpers draw in world coordinates; the engine brackets the complete render hook. */
final class ModRenderApi(private val camera: () => Vec3):
  def line(x0: Float,y0: Float,z0: Float,x1: Float,y1: Float,z1: Float,r: Float,g: Float,b: Float,a: Float): Unit=
    val c=camera()
    glDisable(GL_TEXTURE_2D); glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    glColor4f(r,g,b,a); glBegin(GL_LINES)
    glVertex3f(x0-c.x,y0,z0-c.z); glVertex3f(x1-c.x,y1,z1-c.z); glEnd()
  def box(x: Float,y: Float,z: Float,size: Float,r: Float,g: Float,b: Float,a: Float): Unit=
    val c=camera(); val x0=x-c.x; val z0=z-c.z; val s=size.max(0.001f).min(1024f)
    glDisable(GL_TEXTURE_2D); glEnable(GL_BLEND); glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA)
    glDisable(GL_CULL_FACE); glColor4f(r,g,b,a)
    val vertices=Array(Vec3(x0,y,z0),Vec3(x0+s,y,z0),Vec3(x0+s,y+s,z0),Vec3(x0,y+s,z0),
      Vec3(x0,y,z0+s),Vec3(x0+s,y,z0+s),Vec3(x0+s,y+s,z0+s),Vec3(x0,y+s,z0+s))
    glBegin(GL_QUADS)
    for i <- Array(0,1,2,3,5,4,7,6,4,0,3,7,1,5,6,2,3,2,6,7,4,5,1,0) do
      val v=vertices(i); glVertex3f(v.x,v.y,v.z)
    glEnd()
