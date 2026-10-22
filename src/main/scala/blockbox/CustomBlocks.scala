package blockbox

import scala.jdk.CollectionConverters.*

/** Stable reserved voxel IDs. Definitions are installed before atlas/mesh creation. */
object CustomBlocks:
  final case class Definition(id: String,name: String,block: Block,backing: Block,solid: Boolean,translucent: Boolean,
    cutout: Boolean,tint: (Float,Float,Float),hardness: Float,emission: Int,texture: Option[String])
  private val table=new Array[Definition](256)
  private val names=new java.util.concurrent.ConcurrentHashMap[String,Block]()
  @volatile private var sealedRegistry=false
  def isSlot(block: Block): Boolean = block.ordinal>=Block.Custom00.ordinal && block.ordinal<=Block.Custom31.ordinal
  def definition(block: Block): Option[Definition] = Option(table(block.ordinal))
  def raw(block: Block): Definition = table(block.ordinal)
  def registered(block: Block): Boolean = !isSlot(block) || table(block.ordinal)!=null
  def find(id: String): Option[Block] = Option(names.get(Option(id).getOrElse("").toLowerCase))
  def manifest: String = table.iterator.filter(_!=null).map(d => s"${d.block.ordinal}=${d.id}").mkString(";")
  def seal(): Unit = sealedRegistry=true
  private[blockbox] def checkpoint(): () => Unit =
    val saved=table.clone()
    () => synchronized {
      System.arraycopy(saved,0,table,0,table.length)
      names.clear()
      table.iterator.filter(_!=null).foreach(d => names.put(d.id,d.block))
    }

  def register(id: String,name: String,options: java.util.Map[String,?]): Definition = synchronized {
    require(!sealedRegistry,"Register custom blocks during onInit, before the first world or atlas is created")
    val key=Option(id).getOrElse("").toLowerCase
    require(key.matches("[a-z0-9_.-]+:[a-z0-9_./-]+"),"Custom block IDs must be namespaced, for example demo:marble")
    require(!names.containsKey(key),s"Duplicate custom block ID: $key")
    def value(k: String): Option[Any] = Option(options).flatMap(m => Option(m.get(k)))
    def number(k: String,default: Float): Float = value(k).collect { case n: Number => n.floatValue() }.filter(_.isFinite).getOrElse(default)
    def flag(k: String,default: Boolean): Boolean = value(k).collect { case b: java.lang.Boolean => b.booleanValue() }.getOrElse(default)
    val slot=number("slot",Math.floorMod(key.hashCode,32).toFloat).toInt
    require(slot>=0 && slot<32,"Custom cube slot must be in 0..31")
    val block=Block.fromOrdinal(35+slot)
    require(table(block.ordinal)==null,s"Custom block slot $slot is occupied; assign a different explicit slot in registerCube options")
    val backing=value("backing").flatMap(v => Block.find(v.toString)).filter(b => !isSlot(b) && b!=Block.Air && b!=Block.Water && b!=Block.Torch).getOrElse(Block.Stone)
    val tint=(number("red",1f).max(0f).min(1f),number("green",1f).max(0f).min(1f),number("blue",1f).max(0f).min(1f))
    val texture=value("texture").map(_.toString).filter(_.nonEmpty)
    texture.foreach(t => require(!t.contains("..") && !t.startsWith("/") && !t.contains('\\'),"Texture must be a relative path inside mods/"))
    val d=Definition(key,Option(name).filter(_.nonEmpty).getOrElse(key),block,backing,flag("solid",true),flag("translucent",backing.translucent),
      flag("cutout",backing.cutout),tint,number("hardness",1f).max(0.02f).min(3600f),number("light",0f).toInt.max(0).min(15),texture)
    table(block.ordinal)=d; names.put(key,block)
    d
  }
