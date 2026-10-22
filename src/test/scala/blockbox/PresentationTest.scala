package blockbox

import java.nio.file.{Files,Path}
import java.io.ByteArrayInputStream
import javax.sound.sampled.{AudioFormat,AudioInputStream,AudioFileFormat,AudioSystem}
import scala.collection.mutable.ArrayBuffer
import scala.math.*

object PresentationTest:
  def run(): Unit =
    handAttachments(); atmosphere(); audio(); weatherAndBiomes()

  def weatherAndBiomes(): Unit =
    val storms=WeatherField.cells(8765,400,0,0,1f)
    assert(storms.nonEmpty && storms==WeatherField.cells(8765,400,0,0,1f),"storm cells agree between peers")
    assert(WeatherField.cells(8765,400,0,0,0).isEmpty)
    val storm=storms.minBy(c => c.x*c.x+c.z*c.z)
    assert(WeatherField.sample(storms,storm.x,storm.z).rain>0.9f)
    val transect=(-12 to 12).map(i => WeatherField.sample(storms,storm.x+i*40,storm.z).rain)
    assert(transect.min<0.05f && transect.max>0.9f,"rain is localized with clear ground outside")
    val edge=(0 to 1000).map(i => WeatherField.sample(Seq(storm),storm.x+i*0.5f,storm.z).rain)
    assert(edge.sliding(2).forall(p => abs(p(0)-p(1))<0.01f),"no hard storm boundary")
    val strikes=(0 to 80).flatMap(t => WeatherField.strikes(storms,t.toFloat))
    assert(strikes.nonEmpty && strikes.forall(s => s.age>=0 && s.age<7))
    val gen=TerrainGenerator(8675309)
    val required=Set(TerrainBiome.RockyCoast,TerrainBiome.DuneDesert,TerrainBiome.SnowyTaiga,TerrainBiome.RedwoodForest)
    val found=scala.collection.mutable.Map.empty[TerrainBiome,(Int,Int,Int)]
    for x <- -8192 to 8192 by 128; z <- -8192 to 8192 by 128 do
      val h=gen.heightAt(x,z); val biome=gen.biomeAt(x,z,h)
      if required(biome) && !found.contains(biome) then found(biome)=(x,h,z)
    assert(required.subsetOf(found.keySet.toSet),s"new biomes reachable from real terrain: ${found.keys.mkString(",")}")
    sys.env.get("BLOCKBOX_RENDER_ARTIFACTS").foreach { directory =>
      Files.writeString(Path.of(directory).resolve("biome-locations.csv"),"seed,biome,x,y,z\n"+found.toVector.sortBy(_._1.ordinal).map { case (biome,(x,y,z)) => s"8675309,$biome,$x,$y,$z" }.mkString("\n")+"\n")
    }
    println("[weather/terrain] localized deterministic storm fronts and four new biome regions passed")

  private def rotate(v: Vec3,a: Vec3): Vec3 =
    val z=toRadians(a.z); val y=toRadians(a.y); val x=toRadians(a.x)
    val rz=Vec3((v.x*cos(z)-v.y*sin(z)).toFloat,(v.x*sin(z)+v.y*cos(z)).toFloat,v.z)
    val ry=Vec3((rz.x*cos(y)+rz.z*sin(y)).toFloat,rz.y,(-rz.x*sin(y)+rz.z*cos(y)).toFloat)
    Vec3(ry.x,(ry.y*cos(x)-ry.z*sin(x)).toFloat,(ry.y*sin(x)+ry.z*cos(x)).toFloat)

  def handAttachments(): Unit =
    val cube=FirstPersonPose.attachment(Block.Stone)
    val half=cube.size/2
    val corners=for x <- Seq(-half,half); y <- Seq(-half,half); z <- Seq(-half,half) yield rotate(Vec3(x,y,z),cube.rotation)+cube.position
    assert(corners.map(_.z).min > 0.112f,"held cube cannot intersect the avatar's fist")
    val torch=FirstPersonPose.attachment(Block.Torch)
    def held(x: Float,y: Float,z: Float): Vec3 =
      rotate(Vec3(x-0.5f,y-FirstPersonPose.torchGripHeight,z-0.5f)*torch.size,torch.rotation)+torch.position
    def insideFist(p: Vec3): Boolean = math.abs(p.x)<0.1125f && math.abs(p.z)<0.1125f && p.y > -0.36f && p.y < -0.25f
    val (fx,fy,fz)=TorchModel.flameCenter(TorchMount.Floor)
    assert(insideFist(held(0.5f,FirstPersonPose.torchGripHeight,0.5f)),"the fist closes around the torch handle")
    assert(held(0.5f,0.625f,0.5f).y < -0.40f && !insideFist(held(fx,fy,fz)),"the burning end clears the hand")
    for block <- Seq(Block.Air,Block.Stone,Block.Torch); kick <- Seq(0f,0.25f,0.6f,1f); blocked <- Seq(0f,0.5f,1f) do
      val pose=FirstPersonPose.sample(block,kick,1.5f,1f,blocked)
      assert(pose.position.z < -0.7f && pose.position.x.isFinite && pose.rotation.x.isFinite)
    val open=FirstPersonPose.obstruction(Vec3(0.5f,41.6f,2.5f),0,0,(_,_,_) => Block.Air)
    val wall=FirstPersonPose.obstruction(Vec3(0.5f,41.6f,2.5f),0,0,(_,_,z) => if z==1 then Block.Stone else Block.Air)
    val side=FirstPersonPose.obstruction(Vec3(0.5f,41.6f,2.5f),0,0,(x,_,_) => if x==1 then Block.Stone else Block.Air)
    assert(open==0 && wall>0.9f && side>0.9f,"front and side walls retract the hand")
    val free=FirstPersonPose.sample(Block.Stone,0,0,0,0); val close=FirstPersonPose.sample(Block.Stone,0,0,0,1)
    assert(close.position.z>free.position.z && close.position.y<free.position.y,"obstruction pulls the complete rig toward the camera and down")

  def atmosphere(): Unit =
    for time <- Seq(0f,150f,580f,820f,1070f); storm <- Seq(0f,0.7f,1f) do
      val phase=BlockboxSky.dayPhase(time)
      val palette=BlockboxAtmosphere.palette(phase,BlockboxSky.daylightFactor(phase),storm,storm,0)
      val samples=(-1000 to 1000).map(i => BlockboxAtmosphere.color(Vec3(0,i/1000f,-1),palette))
      assert(samples.forall(c => Seq(c.x,c.y,c.z).forall(v => v.isFinite && v>=0 && v<=1)))
      val steps=samples.sliding(2).map(pair => (pair(1)-pair(0)).lengthSquared).max
      assert(steps<0.0001f,"continuous atmosphere has no horizon or ring color jump")
      val fog=BlockboxSky.clearColor(false,phase,BlockboxSky.daylightFactor(phase),storm,storm,0)
      assert(abs(fog._1-palette.horizon.x)<0.00001f,"fog and sky use the same horizon palette")

  def audio(): Unit =
    import BlockboxSounds.*
    val metrics=ArrayBuffer("material,action,variant,rms,peak,mean,high_frequency_energy")
    def rms(values: Array[Float]): Float = sqrt(values.iterator.map(v => v.toDouble*v).sum/values.length).toFloat
    def treble(values: Array[Float]): Float =
      sqrt((1 until values.length).iterator.map(i => pow(values(i)-values(i-1),2)).sum/values.length).toFloat/rms(values)
    for material <- Material.values; action <- Seq(Action.Step,Action.Place,Action.Break,Action.Land); variant <- 0 until variants do
      val values=clip(action,material,variant)
      val peak=values.iterator.map(abs).max; val mean=values.iterator.map(_.toDouble).sum/values.length
      assert(values.forall(_.isFinite) && peak<0.77f && rms(values)>0.008f,"sound is audible and leaves mixer headroom")
      assert(abs(mean)<0.007f && values.head==0 && values.last==0,"sound has no DC offset or boundary click")
      assert(clip(action,material,variant+variants) eq values,"finite clip variants are reused")
      metrics+=s"$material,$action,$variant,${rms(values)},$peak,$mean,${treble(values)}"
    val glass=clip(Action.Break,Material.Glass,0); val dirt=clip(Action.Break,Material.Dirt,0)
    assert(treble(glass)>treble(dirt)*1.4f,"glass fragments and soft earth have distinct spectra")
    assert(!java.util.Arrays.equals(clip(Action.Step,Material.Stone,0),clip(Action.Step,Material.Stone,1)),"successive footsteps have distinct transients")
    for action <- Seq(Action.Ui,Action.Hurt,Action.Splash,Action.Thunder) do
      val values=clip(action,Material.Stone,0)
      assert(values.forall(_.isFinite) && values.head==0 && values.last==0 && rms(values)>0.005f)
    sys.env.get("BLOCKBOX_RENDER_ARTIFACTS").foreach { directory =>
      val output=Path.of(directory); Files.createDirectories(output)
      Files.writeString(output.resolve("audio-metrics.csv"),metrics.mkString("\n")+"\n")
      val montage=ArrayBuffer.empty[Float]
      val timeline=ArrayBuffer("seconds,material,action")
      for material <- Seq(Material.Stone,Material.Wood,Material.Grass,Material.Sand,Material.Snow,Material.Glass); action <- Seq(Action.Step,Action.Place,Action.Break) do
        timeline+=f"${montage.length.toDouble/sampleRate}%.3f,$material,$action"
        montage++=clip(action,material,0); montage++=Array.fill[Float]((sampleRate*0.22f).toInt)(0)
      writeWav(output.resolve("audio-materials.wav"),montage.toArray)
      Files.writeString(output.resolve("audio-timeline.csv"),timeline.mkString("\n")+"\n")
    }
    println("[presentation] shared hand attachments, continuous sky and 216 material sound variants passed")

  private[blockbox] def writeWav(path: Path,values: Array[Float]): Unit =
    val bytes=new Array[Byte](values.length*2)
    var i=0
    while i<values.length do
      val sample=(values(i)*Short.MaxValue).toInt
      bytes(i*2)=sample.toByte; bytes(i*2+1)=(sample>>>8).toByte
      i+=1
    val stream=AudioInputStream(ByteArrayInputStream(bytes),AudioFormat(BlockboxSounds.sampleRate.toFloat,16,1,true,false),values.length)
    try AudioSystem.write(stream,AudioFileFormat.Type.WAVE,path.toFile)
    finally stream.close()
