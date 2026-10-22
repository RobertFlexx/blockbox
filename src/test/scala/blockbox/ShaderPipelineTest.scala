//> using dep "org.lwjgl:lwjgl-egl:3.4.1"
package blockbox

import org.lwjgl.BufferUtils
import org.lwjgl.egl.EGL
import org.lwjgl.egl.EGL10.*
import org.lwjgl.egl.EGL12.eglBindAPI
import org.lwjgl.egl.EGL14.{EGL_DEFAULT_DISPLAY,EGL_OPENGL_API,EGL_OPENGL_BIT}
import org.lwjgl.opengl.GL
import org.lwjgl.opengl.GL11.*
import org.lwjgl.opengl.GL20.glUseProgram
import org.lwjgl.opengl.GL30.*
import java.nio.file.{Files,Path}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** Real engine tests in a compatibility-profile EGL pbuffer; no display/socket needed. */
@main def runShaderPipelineTests(output: String*): Unit =
  val root = Path.of(".").toAbsolutePath.normalize()
  val artifacts = output.headOption.map(Path.of(_)).getOrElse(Files.createTempDirectory("blockbox-render-review"))
  Files.createDirectories(artifacts)
  val display = eglGetDisplay(EGL_DEFAULT_DISPLAY)
  assert(display!=EGL_NO_DISPLAY,"EGL display unavailable")
  assert(eglInitialize(display,Array(0),Array(0)),"EGL initialize failed")
  EGL.createDisplayCapabilities(display)
  assert(eglBindAPI(EGL_OPENGL_API))
  val configs = BufferUtils.createPointerBuffer(1); val count = Array(0)
  val attributes = Array(EGL_SURFACE_TYPE,EGL_PBUFFER_BIT,0x3040,EGL_OPENGL_BIT,EGL_RED_SIZE,8,EGL_GREEN_SIZE,8,EGL_BLUE_SIZE,8,EGL_ALPHA_SIZE,8,EGL_DEPTH_SIZE,24,EGL_NONE)
  assert(eglChooseConfig(display,attributes,configs,count) && count(0)>0)
  val savedReview=sys.env.get("BLOCKBOX_REVIEW_WORLD").map(Path.of(_))
  val width = if savedReview.nonEmpty then 1920 else 960
  val height = if savedReview.nonEmpty then 1080 else 640
  val surface = eglCreatePbufferSurface(display,configs.get(0),Array(EGL_WIDTH,width,EGL_HEIGHT,height,EGL_NONE))
  val context = eglCreateContext(display,configs.get(0),EGL_NO_CONTEXT,Array(EGL_NONE))
  assert(surface!=EGL_NO_SURFACE && context!=EGL_NO_CONTEXT)
  assert(eglMakeCurrent(display,surface,surface,context))
  GL.createCapabilities()
  val debug = new org.lwjgl.opengl.GLDebugMessageCallback:
    override def invoke(source: Int,kind: Int,id: Int,severity: Int,length: Int,message: Long,user: Long): Unit =
      if kind==org.lwjgl.opengl.GL43.GL_DEBUG_TYPE_ERROR && source!=org.lwjgl.opengl.GL43.GL_DEBUG_SOURCE_SHADER_COMPILER then
        System.err.println("GL diagnostic: "+org.lwjgl.system.MemoryUtil.memUTF8(message,length))
        Thread.currentThread().getStackTrace.take(18).foreach(frame => System.err.println("  "+frame))
  glEnable(org.lwjgl.opengl.GL43.GL_DEBUG_OUTPUT)
  glEnable(org.lwjgl.opengl.GL43.GL_DEBUG_OUTPUT_SYNCHRONOUS)
  org.lwjgl.opengl.GL43.glDebugMessageCallback(debug,0L)
  println(s"[pipeline-render] ${glGetString(GL_RENDERER)} / ${glGetString(GL_VERSION)}")
  try
    if savedReview.nonEmpty then ShaderPipelineTest.savedWorldFrames(root,savedReview.get,artifacts,width,height)
    else
      ShaderPipelineTest.pipeline(root,width,height)
      ShaderPipelineTest.gameFrames(root,artifacts,width,height)
      ShaderPipelineTest.playerPreview(artifacts,width,height)
      ShaderPipelineTest.atmosphereFrames(artifacts,width,height)
      ShaderPipelineTest.polishFrames(root,artifacts,width,height)
      ShaderPipelineTest.skyPreview(artifacts,width,height)
    println(s"[pipeline-render] ${if savedReview.nonEmpty then "saved gameplay review" else "all engine render checks"} passed; previews in $artifacts")
  finally
    org.lwjgl.opengl.GL43.glDebugMessageCallback(null,0L)
    debug.free()
    eglMakeCurrent(display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT)
    eglDestroyContext(display,context); eglDestroySurface(display,surface); eglTerminate(display)

object ShaderPipelineTest:
  /** Read-only review of a real saved camera, terrain, preferences and weather.
    * Shader selection lives in a temporary pack root; no save/settings are written.
    */
  def savedWorldFrames(root: Path,world: Path,output: Path,width: Int,height: Int): Unit =
    val game=Blockbox()
    val preferences=game.getClass.getDeclaredMethod("loadUserSettings"); preferences.setAccessible(true); preferences.invoke(game)
    val packMethod=game.getClass.getDeclaredMethod("selectedTexturePack"); packMethod.setAccessible(true)
    val atlas=TextureAtlas(packMethod.invoke(game).asInstanceOf[TexturePack],get[Boolean](game,"anisotropicFiltering"))
    set(game,"textureAtlas",atlas)
    val chunks=get[mutable.AnyRefMap[(Int,Int),Chunk]](game,"chunks")
    var savedChunks=Vector.empty[(Int,Int)]
    blockbox.io.BlockboxSaveFiles.readWorldData(world,in =>
      set(game,"worldSeed",in.readLong())
      set(game,"camera",Vec3(in.readFloat(),in.readFloat(),in.readFloat()))
      set(game,"yaw",in.readFloat()); set(game,"pitch",in.readFloat())
      set(game,"gameMode",GameMode.fromOrdinal(in.readByte().toInt)); set(game,"worldCheatsEnabled",in.readBoolean())
      val count=in.readInt(); require(count>=0 && count<=200000,"Invalid saved chunk count")
      savedChunks=Vector.fill(count)((in.readInt(),in.readInt()))
      val inventory=game.getClass.getDeclaredMethod("readWorldExtras",classOf[java.io.DataInputStream]); inventory.setAccessible(true); inventory.invoke(game,in)
      if in.available()>=4 && in.readInt()==0x454E5631 then
        val env=blockbox.net.BlockboxNet.parseEnvironment(blockbox.net.BlockboxNet.packet(in.readUTF()))
        set(game,"worldClock",env.time.toDouble); set(game,"timeOverride",if env.timeOverride<0 then None else Some(env.timeOverride))
        set(game,"rainStrength",env.rain); set(game,"wetness",env.wetness); set(game,"stormCloudCover",env.cloudCover)
        set(game,"weatherWind",env.wind)
    )
    val seed=get[Long](game,"worldSeed"); val gen=TerrainGenerator(seed)
    set(game,"terrainGen",gen)
    val camera=get[Vec3](game,"camera"); val distance=get[Int](game,"renderDistance")*Terrain.chunkSize+32
    val temporary=Files.createTempDirectory("blockbox-saved-review")
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("assets/shaders"),temporary.resolve("assets/shaders"))
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("shaderpacks"),temporary.resolve("shaderpacks"))
    val pipeline=BlockboxShaderPipeline(temporary.toFile); pipeline.initialize()
    val fovea=BlockboxFovea(); pipeline.setFovea(fovea)
    set(game,"shaderPipeline",pipeline); set(game,"foveaPass",fovea); set(game,"screen",Screen.Playing)
    set(game,"viewmodelYaw",get[Float](game,"yaw")); set(game,"viewmodelPitch",get[Float](game,"pitch"))
    try
      savedChunks.filter { (cx,cz) =>
        val dx=cx*16+8-camera.x; val dz=cz*16+8-camera.z
        dx*dx+dz*dz<distance*distance
      }.foreach { (cx,cz) =>
        val chunk=Chunk(cx,cz,atlas,gen,smoothLightingEnabled=get[Boolean](game,"smoothLighting"))
        chunk.load(world.resolve("chunks").toFile)
        chunks((cx,cz))=chunk
      }
      chunks.values.foreach { c =>
        for dx <- -1 to 1; dz <- -1 to 1 if dx!=0 || dz!=0 do chunks.get((c.cx+dx,c.cz+dz)).foreach(c.setNeighbor(dx,dz,_))
      }
      chunks.values.foreach(c => assert(c.forceBuildNowAndUpload()))
      val depths=mutable.HashMap.empty[Int,Int]
      for x <- camera.x.toInt-32 to camera.x.toInt+32; z <- camera.z.toInt-32 to camera.z.toInt+32 do
        chunks.get((Math.floorDiv(x,16),Math.floorDiv(z,16))).foreach { c =>
          val lx=Math.floorMod(x,16); val lz=Math.floorMod(z,16)
          var y=Terrain.worldHeight-1
          while y>0 && c.getBlock(lx,y,lz)!=Block.Water do y-=1
          if y>0 then
            val top=y
            while y>0 && c.getBlock(lx,y,lz)==Block.Water do y-=1
            depths.update(top-y,depths.getOrElse(top-y,0)+1)
        }
      println(s"[saved-review] nearby water columns by depth=${depths.toSeq.sortBy(_._1).mkString(",")} cutout vertices=${chunks.valuesIterator.map(c => get[Int](c,"cutoutCount").toLong).sum}")
      val time=get[Double](game,"worldClock").toFloat
      val rain=get[Float](game,"rainStrength")
      val cells=WeatherField.cells(seed,time,camera.x,camera.z,rain,get[Int](game,"cloudRenderDistance")*16f)
      val local=WeatherField.sample(cells,camera.x,camera.z)
      set(game,"weatherCells",cells); set(game,"weatherStrikes",WeatherField.strikes(cells,time))
      set(game,"localRain",local.rain); set(game,"localCloud",local.cloud); set(game,"localWetness",get[Float](game,"wetness")*local.cloud)
      println(s"[saved-review] ${world.getFileName}: seed=$seed eye=$camera fov=${get[Float](game,"fov")} chunks=${chunks.size} time=$time")
      for pack <- Seq("ultra","ray-traced") do
        assert(pipeline.selectPack(pack))
        for _ <- 0 until 10 do game.renderFrame(width,height)
        if pack=="ray-traced" && sys.env.get("BLOCKBOX_REQUIRE_HARDWARE_RT").contains("1") then assert(BlockboxRayTrace.frameReady,BlockboxRayTrace.status)
        capture(output.resolve("saved-world-"+pack+".png"),width,height)
        noError("saved world full gameplay frame "+pack)
        println(s"[saved-review] $pack reflection=${pipeline.hasWaterReflection} hardware=${BlockboxRayTrace.frameReady}")
        val savedTime=get[Option[Float]](game,"timeOverride")
        for (label,clock) <- Seq(("day",150f),("sunset",575f)) do
          set(game,"timeOverride",Some(clock))
          for _ <- 0 until 10 do game.renderFrame(width,height)
          if pack=="ray-traced" && sys.env.get("BLOCKBOX_REQUIRE_HARDWARE_RT").contains("1") then assert(BlockboxRayTrace.frameReady,BlockboxRayTrace.status)
          capture(output.resolve(s"saved-world-$label-$pack.png"),width,height)
          noError(s"saved world $label $pack")
        set(game,"timeOverride",savedTime)
    finally
      chunks.values.foreach(_.dispose()); chunks.clear(); pipeline.destroy(); atlas.destroy(); fovea.release()
      get[FirstPersonPass](game,"firstPersonPass").destroy(); BlockboxSky.destroy(); TorchEffects.destroy(); BlockboxSkin.dispose()
      RainRenderer.destroy(); UnderwaterEffect.destroy()
      blockbox.io.BlockboxFiles.deleteRecursive(temporary)

  def polishFrames(root: Path,output: Path,width: Int,height: Int): Unit =
    val recorded=Files.walk(root.resolve("assets/audio"))
    val clips=try
      import scala.jdk.CollectionConverters.*
      recorded.iterator().asScala.filter(_.toString.endsWith(".ogg")).map { path =>
        val name=root.resolve("assets/audio").relativize(path).toString
        val values=RecordedAudio.load(name,loop=name.contains("rain-loop")).getOrElse(throw AssertionError("Could not decode "+name))
        assert(values.forall(_.isFinite) && values.iterator.map(math.abs).max<=0.66f,name)
        assert(RecordedAudio.load(name,loop=name.contains("rain-loop")).get eq values,"recordings are reused")
        name->values
      }.toMap
    finally recorded.close()
    assert(clips.size==47)
    val rain=clips("weather/rain-loop.ogg")
    assert(math.abs(rain.head-rain.last)<0.025f,"decoded ambience has a smooth loop boundary")
    PresentationTest.writeWav(output.resolve("recorded-footsteps.wav"),Seq("grass","concrete","wood","snow").flatMap { material =>
      (0 until 3).flatMap(i => clips(f"kenney/footstep_${material}_${i}%03d.ogg").toVector++Vector.fill(10000)(0f))
    }.toArray)
    PresentationTest.writeWav(output.resolve("recorded-weather.wav"),rain.take(44100*8)++Array.fill(15000)(0f)++clips("weather/thunder-distant.ogg"))
    PresentationTest.writeWav(output.resolve("recorded-zombies.wav"),Seq("idle_0","idle_1","idle_2","attack_0","hurt_0","hurt_1","death_0","death_1")
      .flatMap(name => clips(s"zombie/$name.ogg").toVector++Vector.fill(12000)(0f)).toArray)
    assert(RecordedAudio.load("../outside.ogg").isEmpty)
    val temporary=Files.createTempDirectory("blockbox-sky-polish")
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("assets/shaders"),temporary.resolve("assets/shaders"))
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("shaderpacks"),temporary.resolve("shaderpacks"))
    val pipeline=BlockboxShaderPipeline(temporary.toFile,Some("default")); pipeline.initialize()
    assert(pipeline.selectPack("ultra"),"UI selection overrides the launch pack")
    try
      for (name,time,moon) <- Seq(("sun-hdr",150f,false),("sunrise-hdr",12f,false),("moon-hdr",850f,true),("moon-crescent-hdr",850f+2160f,true)) do
        val phase=BlockboxSky.dayPhase(time); val daylight=BlockboxSky.daylightFactor(phase)
        val direction=if moon then BlockboxSky.moonDirection(phase) else BlockboxSky.sunDirection(phase)
        val yaw=math.toDegrees(math.atan2(direction._1,-direction._3)).toFloat
        val pitch= -math.toDegrees(math.asin(direction._2)).toFloat
        assert(pipeline.prepareFrame(width,height)); pipeline.clearPreparedFrame(0,0,0)
        glDisable(GL_FOG); glDisable(GL_TEXTURE_2D); glDisable(GL_DEPTH_TEST)
        glMatrixMode(GL_PROJECTION); glLoadIdentity()
        val top=math.tan(math.toRadians(70)/2)*0.25
        glFrustum(-top*width/height,top*width/height,-top,top,0.25,1024)
        glMatrixMode(GL_MODELVIEW); glLoadIdentity()
        BlockboxSky.renderSky(yaw,pitch,phase,daylight,BlockboxSky.generateStars(),false,Vec3(0,60,0),time,512,0,0,0,false)
        pipeline.finishFrame(width,height,time)
        capture(output.resolve(name+".png"),width,height); noError(name)
      val time=400f; val cells=WeatherField.cells(8765,time,0,0,1f,1000)
      val storm=cells.minBy(c => c.x*c.x+c.z*c.z)
      val camera=Vec3(storm.x+storm.radius*1.15f,85f,storm.z+storm.radius*0.9f)
      val direction=Vec3(storm.x-camera.x,100,storm.z-camera.z).normalized
      val yaw=math.toDegrees(math.atan2(direction.x,-direction.z)).toFloat
      val pitch= -math.toDegrees(math.asin(direction.y)).toFloat
      val phase=BlockboxSky.dayPhase(150); val daylight=BlockboxSky.daylightFactor(phase)
      glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
      glMatrixMode(GL_MODELVIEW); glLoadIdentity(); glRotatef(pitch,1,0,0); glRotatef(yaw,0,1,0); glTranslatef(0,-camera.y,0)
      BlockboxSky.renderSky(yaw,pitch,phase,daylight,BlockboxSky.generateStars(),true,camera,time,900,0,0,0)
      BlockboxSky.renderClouds(camera,time,900,phase,daylight,1f,0,8765)
      BlockboxWeather.render(camera,time,daylight,cells,Seq.empty,900)
      capture(output.resolve("localized-storm.png"),width,height); noError("localized storm")
      val atlas=TextureAtlas(TexturePack("review","Review","Blockbox",Some(root.resolve("assets/textures").toFile)))
      val game=Blockbox(); val gen=TerrainGenerator(8675309)
      val chunks=get[mutable.AnyRefMap[(Int,Int),Chunk]](game,"chunks")
      val renderer=game.getClass.getDeclaredMethod("renderWorld"); renderer.setAccessible(true)
      set(game,"textureAtlas",atlas); set(game,"terrainGen",gen); set(game,"shaderPipeline",pipeline)
      set(game,"renderDistance",5); set(game,"cloudRenderDistance",24); set(game,"foveatedRendering",false)
      set(game,"framebufferWidth",width); set(game,"framebufferHeight",height); set(game,"worldClock",150.0)
      assert(pipeline.selectPack("ultra"))
      try
        import scala.jdk.CollectionConverters.*
        for row <- Files.readAllLines(output.resolve("biome-locations.csv")).asScala.drop(1) do
          val fields=row.split(','); val x=fields(2).toInt; val y=fields(3).toInt; val z=fields(4).toInt
          val cx=Math.floorDiv(x,16); val cz=Math.floorDiv(z,16)
          for dx <- -2 to 2; dz <- -2 to 2 do chunks((cx+dx,cz+dz))=Chunk(cx+dx,cz+dz,atlas,gen)
          chunks.values.foreach { c =>
            for dx <- -1 to 1; dz <- -1 to 1 if dx!=0 || dz!=0 do chunks.get((c.cx+dx,c.cz+dz)).foreach(c.setNeighbor(dx,dz,_))
          }
          chunks.values.foreach(c => assert(c.forceBuildNowAndUpload()))
          // Tall forests would otherwise put the camera inside a canopy.
          def topAt(wx: Int,wz: Int): Int =
            chunks.get((Math.floorDiv(wx,16),Math.floorDiv(wz,16))).map { c =>
              var h=Terrain.worldHeight-1
              while h>0 && c.getBlock(Math.floorMod(wx,16),h,Math.floorMod(wz,16))==Block.Air do h-=1
              h
            }.getOrElse(y)
          val clearance=(for dx <- -3 to 3; dz <- -3 to 3 yield topAt(x+8+dx,z+24+dz)).max+5
          val eye=(y+14).max(clearance)
          set(game,"camera",Vec3(x+8,eye.toFloat,z+24)); set(game,"yaw",0f); set(game,"pitch",if eye>y+20 then 26f else 16f)
          assert(pipeline.prepareFrame(width,height)); pipeline.clearPreparedFrame(0.6f,0.75f,0.9f)
          renderer.invoke(game); pipeline.finishFrame(width,height,150)
          capture(output.resolve("biome-"+fields(1).toLowerCase+".png"),width,height)
          noError("biome "+fields(1))
          chunks.values.foreach(_.dispose()); chunks.clear()
      finally
        chunks.values.foreach(_.dispose()); atlas.destroy()
      println("[polish-render] 47 recordings, loop continuity, HDR sun/moon and distant storm passed")
    finally pipeline.destroy(); BlockboxSky.destroy(); blockbox.io.BlockboxFiles.deleteRecursive(temporary)

  private def readPixel(): Array[Float] =
    val pixel = BufferUtils.createFloatBuffer(4)
    glReadPixels(16,16,1,1,GL_RGBA,GL_FLOAT,pixel)
    Array.tabulate(4)(pixel.get)

  private def noError(label: String): Unit =
    val error = glGetError()
    assert(error==GL_NO_ERROR,f"$label: OpenGL error 0x$error%x")

  def pipeline(root: Path,width: Int,height: Int): Unit =
    val temporary = Files.createTempDirectory("blockbox-pipeline-test")
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("assets/shaders"),temporary.resolve("assets/shaders"))
    val fixture = Files.createDirectories(temporary.resolve("shaderpacks/fixture/shaders"))
    val vertex = "#version 120\nvoid main(){gl_Position=ftransform();}\n"
    Files.writeString(fixture.resolve("gbuffers_terrain.vsh"),vertex)
    Files.writeString(fixture.resolve("gbuffers_terrain.fsh"),"#version 120\nvoid main(){gl_FragData[0]=vec4(0.2,0.3,0.4,1.0);}\n")
    Files.writeString(fixture.resolve("deferred.vsh"),vertex)
    Files.writeString(fixture.resolve("deferred.fsh"),"#version 120\n/* RENDERTARGETS: 0,4 */\nconst int colortex4Format=RGBA16F;\nvoid main(){gl_FragData[0]=vec4(0.1);gl_FragData[1]=vec4(2.0,0.5,0.25,1.0);}\n")
    Files.writeString(fixture.resolve("composite.vsh"),vertex)
    Files.writeString(fixture.resolve("composite.fsh"),"#version 120\nuniform sampler2D colortex4;\nvoid main(){gl_FragColor=texture2D(colortex4,vec2(0.5))*vec4(0.4,0.6,0.6,1.0);}\n")
    Files.writeString(temporary.resolve("shaderpacks/active.txt"),"fixture\n")
    val pipeline = BlockboxShaderPipeline(temporary.toFile)
    val scale = BlockboxFovea(); scale.internalScale=0.85f
    pipeline.setFovea(scale)
    pipeline.initialize()
    try
      assert(pipeline.available && pipeline.postProcessing,pipeline.status)
      assert(!pipeline.shadowMapping,"unused shadow maps disabled")
      assert(pipeline.prepareFrame(width,height))
      val viewport = BufferUtils.createIntBuffer(4); glGetIntegerv(GL_VIEWPORT,viewport)
      assert(viewport.get(2)==(width*0.85f).round && viewport.get(3)==(height*0.85f).round,"scaling applies to scene and post work")
      pipeline.clearPreparedFrame(0.2f,0.3f,0.4f)
      for attachment <- 1 to 4 do
        glReadBuffer(GL_COLOR_ATTACHMENT0+attachment)
        val actual = readPixel()
        val expected = if attachment==1 then Array(0.5f,0.5f,1f,1f) else Array(0f,0f,0f,0f)
        assert(actual.zip(expected).forall((a,b) => math.abs(a-b)<0.005f),s"attachment $attachment not cleared: ${actual.toList}")
      glReadBuffer(GL_COLOR_ATTACHMENT0)
      pipeline.finishFrame(width,height,150f)
      glReadBuffer(GL_BACK)
      val hdr = readPixel()
      assert(math.abs(hdr(0)-0.8f)<0.02f && math.abs(hdr(1)-0.3f)<0.02f && math.abs(hdr(2)-0.15f)<0.02f,s"HDR ping-pong lost data: ${hdr.toList}")
      noError("HDR pipeline")
      scale.enabled=false
      assert(pipeline.prepareFrame(width,height))
      glGetIntegerv(GL_VIEWPORT,viewport)
      assert(viewport.get(2)==width && viewport.get(3)==height,"native resolution restored")
      pipeline.clearPreparedFrame(0,0,0); pipeline.finishFrame(width,height,150)
      // A failed pack reload must retain the previous programs and pack identity.
      val broken = Files.createDirectories(temporary.resolve("shaderpacks/broken/shaders"))
      Files.writeString(broken.resolve("gbuffers_terrain.fsh"),"#version 120\ninvalid shader text\n")
      Files.writeString(temporary.resolve("shaderpacks/active.txt"),"broken\n")
      pipeline.reloadIfChanged(10)
      assert(pipeline.packName=="fixture" && pipeline.available,pipeline.status)
      assert(pipeline.prepareFrame(width,height)); pipeline.clearPreparedFrame(0,0,0); pipeline.finishFrame(width,height,150)
      val retained = readPixel()
      assert(math.abs(retained(0)-0.8f)<0.02f,"working output retained after shader error")
      noError("failed reload recovery")
      blockbox.io.BlockboxFiles.copyRecursive(root.resolve("shaderpacks"),temporary.resolve("shaderpacks"))
      for _ <- 0 until 2; pack <- Vector("ultra","default","ray-traced","default","ultra","default","ray-traced") do
        assert(pipeline.selectPack(pack),s"switch to $pack: ${pipeline.status}")
        assert(pipeline.packName==pack && pipeline.prepareFrame(width,height))
        pipeline.clearPreparedFrame(0.28f,0.32f,0.36f)
        glMatrixMode(GL_PROJECTION); glLoadIdentity()
        glMatrixMode(GL_MODELVIEW); glLoadIdentity()
        if pipeline.beginShadow((0f,60f,0f),(0.5f,0.8f,0.2f),(-0.5f,-0.8f,-0.2f),1f,128f,150f,0,0) then
          pipeline.setShadowPass(1)
          pipeline.endShadow()
        assert(pipeline.beginWorld(0.6f,0.7f,0.8f,100f,150f,1f,150f,(0.5f,0.8f,0.2f),(-0.5f,-0.8f,-0.2f),(0f,60f,0f),0.25f,512f,0,0,false))
        for pass <- 0 to 3 do pipeline.setPass(pass)
        pipeline.end()
        pipeline.finishFrame(width,height,150)
        assert(readPixel().take(3).forall(v => v.isFinite && v>0.01f),s"$pack rendered black")
        noError(s"switch to $pack")
      assert(pipeline.selectPack("ultra") && pipeline.prepareFrame(width,height))
      pipeline.clearPreparedFrame(0,0,0)
      glMatrixMode(GL_PROJECTION); glLoadIdentity(); glOrtho(-4,4,-4,4,0.25,100)
      glMatrixMode(GL_MODELVIEW); glLoadIdentity(); glTranslatef(0,-62,-10)
      assert(pipeline.beginWorld(0.6f,0.7f,0.8f,100,150,1,150,(0.5f,0.8f,0.2f),(-0.5f,-0.8f,-0.2f),(0,62,0),0.25f,100,0,0,false))
      val before=BufferUtils.createFloatBuffer(16); glGetFloatv(GL_MODELVIEW_MATRIX,before)
      val sceneTarget=glGetInteger(GL_FRAMEBUFFER_BINDING)
      assert(pipeline.captureWaterReflection(60) {
        pipeline.end(); glDisable(GL_TEXTURE_2D); glDisable(GL_LIGHTING); glDisable(GL_ALPHA_TEST)
        def reflectedQuad(y: Float): Unit =
          glBegin(GL_QUADS)
          glVertex3f(-2,y,0); glVertex3f(2,y,0); glVertex3f(2,y+1,0); glVertex3f(-2,y+1,0)
          glEnd()
        glColor4f(0,1,0,1); reflectedQuad(60.5f)
        glColor4f(1,0,0,1); reflectedQuad(58.5f)
      })
      assert(pipeline.hasWaterReflection && glGetInteger(GL_FRAMEBUFFER_BINDING)==sceneTarget,"capture restores the scene target")
      val after=BufferUtils.createFloatBuffer(16); glGetFloatv(GL_MODELVIEW_MATRIX,after)
      assert((0 until 16).forall(i => before.get(i)==after.get(i)),"capture restores the camera transform")
      val reflected=BufferUtils.createFloatBuffer((width/2)*(height/2)*4)
      glBindTexture(GL_TEXTURE_2D,get[Int](pipeline,"reflectionColor"))
      glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_FLOAT,reflected)
      var above=0; var below=0
      for i <- 0 until (width/2)*(height/2) do
        if reflected.get(i*4+1)>0.9f then above+=1
        if reflected.get(i*4)>0.9f then below+=1
      assert(above>100 && below==0,"the mirror includes above-water objects and clips submerged geometry")
      glBindTexture(GL_TEXTURE_2D,0); pipeline.end(); pipeline.finishFrame(width,height,150)
      assert(pipeline.prepareFrame(width,height) && !pipeline.hasWaterReflection,"a previous reflection cannot leak into the next frame")
      pipeline.clearPreparedFrame(0,0,0); pipeline.finishFrame(width,height,150)
      noError("planar reflection coverage, clipping and state restoration")
      assert(pipeline.selectPack("fixture") && pipeline.prepareFrame(width,height))
      glDepthMask(true); pipeline.clearPreparedFrame(0,0,0)
      glUseProgram(0); glDisable(GL_TEXTURE_2D); glDisable(GL_FOG); glDisable(GL_BLEND)
      glEnable(GL_DEPTH_TEST); glDepthFunc(GL_LESS)
      glMatrixMode(GL_PROJECTION); glLoadIdentity(); glOrtho(-1,1,-1,1,-1,1)
      glMatrixMode(GL_MODELVIEW); glLoadIdentity()
      def depthQuad(z: Float): Unit =
        glBegin(GL_QUADS)
        glVertex3f(-1,-1,z); glVertex3f(1,-1,z); glVertex3f(1,1,z); glVertex3f(-1,1,z)
        glEnd()
      def depthAtCenter(): Float =
        val value=BufferUtils.createFloatBuffer(1)
        glReadPixels(width/2,height/2,1,1,GL_DEPTH_COMPONENT,GL_FLOAT,value); value.get(0)
      depthQuad(0)
      val opaqueDepth=depthAtCenter()
      pipeline.beginWaterSurface()
      depthQuad(0.6f); depthQuad(0.2f) // near water, then far water in the wrong draw order
      assert(math.abs(depthAtCenter()-0.2f)<0.001f,"water keeps the nearest interface independent of face order")
      pipeline.endWaterSurface()
      assert(math.abs(depthAtCenter()-opaqueDepth)<0.0001f,"refraction retains the original opaque depth")
      noError("isolated water depth")
      pipeline.finishFrame(width,height,150)
    finally
      pipeline.destroy(); scale.release()
      blockbox.io.BlockboxFiles.deleteRecursive(temporary)

  private def set(game: Blockbox,name: String,value: Any): Unit =
    val field = game.getClass.getDeclaredFields.find(f => f.getName==name || f.getName.endsWith("$$"+name)).get
    field.setAccessible(true); field.set(game,value)

  private def get[A](game: AnyRef,name: String): A =
    val field = game.getClass.getDeclaredFields.find(f => f.getName==name || f.getName.endsWith("$$"+name)).get
    field.setAccessible(true); field.get(game).asInstanceOf[A]

  def gameFrames(root: Path,output: Path,width: Int,height: Int): Unit =
    val game = Blockbox()
    val gen = TerrainGenerator(8675309L)
    val atlas = TextureAtlas(TexturePack("test","Test","Blockbox",Some(root.resolve("assets/textures").toFile)))
    exportAtlas(atlas,output.resolve("atlas.png"))
    val temporary = Files.createTempDirectory("blockbox-game-frame")
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("assets/shaders"),temporary.resolve("assets/shaders"))
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("shaderpacks"),temporary.resolve("shaderpacks"))
    Files.writeString(temporary.resolve("shaderpacks/active.txt"),"ultra\n")
    val pipeline = BlockboxShaderPipeline(temporary.toFile); pipeline.initialize()
    val fovea = BlockboxFovea(); fovea.enabled=false; pipeline.setFovea(fovea)
    val chunks = get[mutable.AnyRefMap[(Int,Int),Chunk]](game,"chunks")
    try
      for x <- -3 to 3; z <- -3 to 3 do chunks((x,z))=Chunk(x,z,atlas,gen)
      chunks.values.foreach { c =>
        for dx <- -1 to 1; dz <- -1 to 1 if dx!=0 || dz!=0 do chunks.get((c.cx+dx,c.cz+dz)).foreach(c.setNeighbor(dx,dz,_))
      }
      chunks.values.foreach(c => assert(c.forceBuildNowAndUpload()))
      noError("chunk mesh upload")
      set(game,"textureAtlas",atlas); set(game,"terrainGen",gen)
      set(game,"shaderPipeline",pipeline); set(game,"foveaPass",fovea); set(game,"foveatedRendering",false)
      set(game,"framebufferWidth",width); set(game,"framebufferHeight",height)
      set(game,"renderDistance",5); set(game,"cloudRenderDistance",24)
      set(game,"screen",Screen.Playing); set(game,"worldClock",150.0)
      val render = game.getClass.getDeclaredMethod("renderWorld"); render.setAccessible(true)
      def frame(name: String,position: Vec3,yaw: Float,pitch: Float,clouds: Boolean): Unit =
        set(game,"camera",position); set(game,"yaw",yaw); set(game,"pitch",pitch); set(game,"cloudsEnabled",clouds)
        game.renderFrame(width,height)
        if pitch==90f then assert(get[ArrayBuffer[Chunk]](game,"renderChunkScratch").length==49,"downward camera retains all loaded chunks at every yaw")
        noError(name)
        capture(output.resolve(name+".png"),width,height)
      val spawn = BlockboxWorld.findSafeSpawn(gen,(x,y,z) => chunks.get((Math.floorDiv(x,16),Math.floorDiv(z,16))).map(c => c.getBlock(Math.floorMod(x,16),y,Math.floorMod(z,16))))
      frame("terrain",spawn+Vec3(0,10f,0),0,22,true)
      for pack <- Vector("ultra","default","ray-traced") do
        assert(pipeline.selectPack(pack),pipeline.status)
        frame("shader-"+pack,spawn+Vec3(0,10f,0),0,22,true)
      frame("aerial",Vec3(8,300,8),0,90,true)
      frame("aerial-rotated",Vec3(8,300,8),135,90,false)
      set(game,"shadersEnabled",false); set(game,"foveatedRendering",true); fovea.enabled=true
      glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
      render.invoke(game); noError("scaled compatibility renderer")
      val center = BufferUtils.createFloatBuffer(4); val corner = BufferUtils.createFloatBuffer(4)
      glReadPixels(width/2,height/2,1,1,GL_RGBA,GL_FLOAT,center)
      glReadPixels(8,8,1,1,GL_RGBA,GL_FLOAT,corner)
      assert((0 until 3).map(i => math.abs(center.get(i)-corner.get(i))).sum>0.08f,"terrain remains visible during aerial flight with shaders disabled")
      capture(output.resolve("compatibility.png"),width,height)
      // Draw the same shared UI primitives/layout used by the title screen.
      glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
      BlockboxRender2D.setupOrtho(width,height)
      BlockboxRender2D.drawMenuBackdrop(width,height,0)
      val layout = BlockboxUiLayout.mainMenu(width,height,0.9f); val p = layout.panel; val s = layout.scale
      BlockboxRender2D.drawPanel(p.x,p.y,p.w,p.h,s)
      BlockboxRender2D.drawPixelLogo(width/2f,p.y+62*s,"BLOCKBOX",p.w-104*s,62*s)
      BlockboxRender2D.centeredText(width/2f,p.y+142*s,"Find your own way.",0.91f,0.89f,0.78f,1.45f*s,width,height)
      val labels = Vector("Create world","Host a world","Join friends","My worlds","Mods","Textures & shaders","Settings","Quit")
      layout.buttons.zipWithIndex.foreach { case (b,i) => BlockboxRender2D.drawButton(b.x,b.y,b.w,b.h,labels(i),i==0,false,s,width,height) }
      capture(output.resolve("title.png"),width,height)
      noError("title UI")
      torchFrames(game,atlas,pipeline,chunks,output,width,height)
      firstPersonFrames(game,output,width,height)
      heldToolFrames(game,output,width,height)
      materialLightingFrames(game,atlas,pipeline,chunks,output,width,height)
      survivalFrames(game,atlas,pipeline,chunks,output,width,height)
      inventoryFrames(game,output,width,height)
      cinematicFrames(game,atlas,pipeline,chunks,output,width,height)
    finally
      chunks.values.foreach(_.dispose()); atlas.destroy(); pipeline.destroy(); fovea.release()
      get[FirstPersonPass](game,"firstPersonPass").destroy(); BlockboxSky.destroy(); TorchEffects.destroy(); BlockboxSkin.dispose()
      RainRenderer.destroy(); UnderwaterEffect.destroy()
      blockbox.io.BlockboxFiles.deleteRecursive(temporary)

  private def materialLightingFrames(game: Blockbox,atlas: TextureAtlas,pipeline: BlockboxShaderPipeline,
                                     chunks: mutable.AnyRefMap[(Int,Int),Chunk],output: Path,width: Int,height: Int): Unit =
    chunks.values.foreach(_.dispose()); chunks.clear()
    val gen=TerrainGenerator(42)
    val blocks=new Array[Byte](16*16*Terrain.worldHeight)
    blocks((40*16+8)*16+8)=Block.Stone.id
    val cube=Chunk(0,0,atlas,gen,blocks)
    chunks((0,0))=cube; assert(cube.forceBuildNowAndUpload())
    set(game,"terrainGen",gen); set(game,"camera",Vec3(12.5f,43.7f,14.5f)); set(game,"yaw",-34f); set(game,"pitch",24f)
    set(game,"shadersEnabled",true); set(game,"foveatedRendering",false); set(game,"worldClock",150.0)
    assert(pipeline.selectPack("ultra"))
    game.renderFrame(width,height)
    val albedo=BufferUtils.createFloatBuffer(width*height*4)
    val normals=BufferUtils.createFloatBuffer(width*height*4)
    glBindTexture(GL_TEXTURE_2D,get[Array[Int]](pipeline,"sceneExtraColors")(4)); glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_FLOAT,albedo)
    glBindTexture(GL_TEXTURE_2D,get[Int](pipeline,"sceneNormal")); glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_FLOAT,normals)
    glBindTexture(GL_TEXTURE_2D,0)
    val inverse=get[Array[Float]](pipeline,"modelViewInverse")
    val sums=Array.fill(3)(0.0); val counts=Array.fill(3)(0)
    for i <- 0 until width*height if albedo.get(i*4+3)>0.9f do
      val nx=normals.get(i*4)*2-1; val ny=normals.get(i*4+1)*2-1; val nz=normals.get(i*4+2)*2-1
      val worldY=inverse(1)*nx+inverse(5)*ny+inverse(9)*nz
      val worldX=inverse(0)*nx+inverse(4)*ny+inverse(8)*nz
      val face=if worldY>0.8f then 0 else if math.abs(worldX)>0.8f then 1 else 2
      sums(face)+=albedo.get(i*4)*0.2126+albedo.get(i*4+1)*0.7152+albedo.get(i*4+2)*0.0722
      counts(face)+=1
    assert(counts.forall(_>20),s"material fixture must expose three faces: ${counts.toSeq}")
    val means=sums.zip(counts).map((sum,count) => sum/count)
    assert(means.min/means.max>0.78,s"stone albedo is independent of face direction before lighting: ${means.toSeq}")
    capture(output.resolve("material-directional-lighting.png"),width,height); noError("material face shading")
    println("[material-render] three cube faces retain the same albedo before directional lighting")

  /** One lakeshore across lighting/weather changes, with real shadow casters,
    * transparent water, torch sources and a roof. This catches palette regressions
    * that isolated shader compilation or a flat fullbright quad cannot show.
    */
  private def cinematicFrames(game: Blockbox,atlas: TextureAtlas,pipeline: BlockboxShaderPipeline,
                              chunks: mutable.AnyRefMap[(Int,Int),Chunk],output: Path,width: Int,height: Int): Unit =
    chunks.values.foreach(_.dispose()); chunks.clear()
    val gen=TerrainGenerator(42)
    set(game,"terrainGen",gen); set(game,"renderDistance",8); set(game,"cloudRenderDistance",32)
    set(game,"screen",Screen.Playing); set(game,"shadersEnabled",true); set(game,"cloudsEnabled",true)
    val tops=mutable.HashMap.empty[(Int,Int),Int]
    for cx <- -4 to 3; cz <- -4 to 3 do
      val blocks=new Array[Byte](16*16*Terrain.worldHeight)
      for x <- 0 until 16; z <- 0 until 16 do
        val wx=cx*16+x; val wz=cz*16+z
        val r=math.sqrt(wx*wx+wz*wz).toFloat
        val top=if r<10 then 22+(r*0.9f).toInt else if r<28 then (31+r/4).toInt.min(38) else
          40+((r-28)*0.29f+math.sin(wx*0.13)*1.4+math.cos(wz*0.1)*1.4).toInt.max(0).min(12)
        tops((wx,wz))=top
        for y <- 0 to top.max(39) do
          val b=if y>top then Block.Water else if y==top then
            (if r<30 then Block.Sand else Block.Grass) else if y>top-3 then Block.Dirt else Block.Stone
          blocks((y*16+z)*16+x)=b.id
      chunks((cx,cz))=Chunk(cx,cz,atlas,gen,blocks)
    def put(x: Int,y: Int,z: Int,b: Block): Unit =
      chunks.get((Math.floorDiv(x,16),Math.floorDiv(z,16))).foreach(_.setBlock(Math.floorMod(x,16),y,Math.floorMod(z,16),b))
    // Explicit water edits survive the world's old-water repair at this altitude.
    for ((x,z),top) <- tops if top<39; y <- top+1 to 39 do put(x,y,z,Block.Water)
    val rng=new scala.util.Random(619)
    for x <- -58 to 54 by 7; z <- -58 to 54 by 7 do
      val r=math.sqrt(x*x+z*z)
      if r>32 && rng.nextFloat()<0.76f && !(x> -40 && x< -20 && z> -20 && z<1) then
        val y=tops((x,z))+1; val h=6+rng.nextInt(4)
        for k <- 0 until h do put(x,y+k,z,Block.Wood)
        for k <- 2 to h+2; dx <- -3 to 3; dz <- -3 to 3 if math.abs(dx)+math.abs(dz)<(if k>h then 3 else 5) do
          if dx!=0 || dz!=0 || k>=h then put(x+dx,y+k,z+dz,Block.Leaves)
    // A lit log cabin gives night light a useful warm subject against the blue sky.
    for x <- -37 to -26; z <- -16 to -6; y <- tops((x,z))+1 to 43 do put(x,y,z,Block.Dirt)
    for x <- -37 to -26; z <- -16 to -6 do
      put(x,44,z,Block.Planks)
      for y <- 45 to 48 do
        val wall=x== -37 || x== -26 || z== -16 || z== -6
        val corner=(x== -37 || x== -26) && (z== -16 || z== -6)
        val window=wall && !corner && (y==46 || y==47) && (z== -12 || z== -11 || x== -32 || x== -31)
        put(x,y,z,if !wall then Block.Air else if window then Block.Glass else if corner then Block.Wood else Block.Planks)
    for x <- -38 to -25; z <- -17 to -5 do
      val y=49+((5.5f-math.abs(x+31.5f))*0.65f).toInt.max(0)
      put(x,y,z,Block.Wood)
    for (x,z) <- Seq((-27,-8),(-27,-14),(-35,-8),(-35,-14)) do put(x,45,z,Block.Torch)
    // Two porch torches; unlike interior sources these are visible in the mirror.
    for z <- Seq(-14,-8) do {put(-25,43,z,Block.Stone); put(-25,44,z,Block.Torch)}
    chunks.values.foreach { c =>
      for dx <- -1 to 1; dz <- -1 to 1 if dx!=0 || dz!=0 do chunks.get((c.cx+dx,c.cz+dz)).foreach(c.setNeighbor(dx,dz,_))
    }
    chunks.values.foreach(c => assert(c.forceBuildNowAndUpload()))
    val flames=game.getClass.getDeclaredMethod("refreshTorchFlames"); flames.setAccessible(true); flames.invoke(game)
    def frame(pack: String,name: String,time: Float,rain: Float,underwater: Boolean=false): Float =
      assert(pipeline.packName==pack || pipeline.selectPack(pack))
      set(game,"camera",if underwater then Vec3(0,31,0) else Vec3(22,43,14))
      set(game,"yaw",if underwater then 73f else -65f); set(game,"pitch",if underwater then -55f else -3f)
      set(game,"worldClock",time.toDouble)
      set(game,"localRain",rain); set(game,"localCloud",rain); set(game,"stormCloudCover",rain)
      set(game,"localWetness",rain); set(game,"weatherWind",0.35f)
      set(game,"weatherCells",Vector(WeatherField.Cell(0,0,260,rain,42)))
      game.renderFrame(width,height)
      assert(pipeline.hasWaterReflection == !underwater,"lake views capture a mirror only above water")
      if pack=="ray-traced" && sys.env.get("BLOCKBOX_REQUIRE_HARDWARE_RT").contains("1") then
        assert(BlockboxRayTrace.frameReady,s"$name must complete a hardware ray frame: ${BlockboxRayTrace.status}")
      capture(output.resolve(s"cinematic-$name-$pack.png"),width,height); noError(s"$pack $name")
      val pixels=BufferUtils.createFloatBuffer(width*height*4)
      glReadPixels(0,0,width,height,GL_RGBA,GL_FLOAT,pixels)
      (0 until width*height).map(i => pixels.get(i*4)*0.2126f+pixels.get(i*4+1)*0.7152f+pixels.get(i*4+2)*0.0722f).sum/(width*height)
    for pack <- Seq("ultra","ray-traced") do
      val day=frame(pack,"day",150,0)
      for i <- 0 until 3 do frame(pack,s"steady-day-$i",150,0)
      val depthPixels=BufferUtils.createFloatBuffer(width*height)
      val actualDepth=BufferUtils.createFloatBuffer(width*height)
      val waterPixels=BufferUtils.createFloatBuffer(width*height*4)
      val aux=get[Array[Int]](pipeline,"sceneExtraColors")
      glBindTexture(GL_TEXTURE_2D,aux(1)); glGetTexImage(GL_TEXTURE_2D,0,GL_RED,GL_FLOAT,depthPixels)
      glBindTexture(GL_TEXTURE_2D,get[Int](pipeline,"waterTestDepth")); glGetTexImage(GL_TEXTURE_2D,0,GL_DEPTH_COMPONENT,GL_FLOAT,actualDepth)
      glBindTexture(GL_TEXTURE_2D,aux(0)); glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_FLOAT,waterPixels)
      glBindTexture(GL_TEXTURE_2D,0)
      val waterSamples=(0 until width*height).filter(i => waterPixels.get(i*4+3)>0.5f)
      assert(waterSamples.size>100,"precision fixture contains a water surface")
      val error=waterSamples.map(i => math.abs(depthPixels.get(i)-actualDepth.get(i))).max
      assert(error<0.0000003f,s"$pack preserves water depth without FP16 banding: $error")
      set(game,"timeOverride",Some(150f))
      val fixedDay=frame(pack,"fixed-day",240,0)
      assert(fixedDay>0.2f,"fixed daylight remains visible independently of the simulation clock")
      set(game,"timeOverride",None)
      val sunset=frame(pack,"sunset",575,0)
      val night=frame(pack,"night",1030,0)
      val storm=frame(pack,"storm",150,1)
      frame(pack,"underwater-sun",150,0,true)
      assert(day>0.2f && sunset>0.08f,"day and golden hour preserve visible terrain")
      assert(night<day*0.7f,"night has its own dark exposure instead of a grey daylight horizon")
      assert(storm<day*0.85f,"rain darkens both the atmosphere and the landscape")
    println("[cinematic-render] both packs passed daylight, sunset, blue night and storm scene checks")

  def playerPreview(output: Path,width: Int,height: Int): Unit =
    glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
    glClearColor(0.12f,0.18f,0.20f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
    BlockboxRender2D.resetGlArraysAndBuffers(); glUseProgram(0)
    glEnable(GL_DEPTH_TEST); glDepthMask(true); glDisable(GL_BLEND); glDisable(GL_FOG); glDisable(GL_TEXTURE_2D)
    glMatrixMode(GL_PROJECTION); glLoadIdentity(); glOrtho(-4,4,-0.35,3.1,0.1,20)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    BlockboxPlayerModel.beginLighting(viewSpace = false)
    for i <- 0 until 5 do
      val heading=if i==4 then 0f else 180f
      val player = RemotePlayer("Preview",Vec3(-3.2f+i*1.6f,1.62f,-5),Vec3(0,0,0),heading,heading,0,0,0,0)
      player.bodyYaw=heading
      if i==1 then { player.gait=1.1f; player.stride=1f }
      if i==2 then { player.yaw=220; player.pitch=35 }
      if i==3 then player.swing=0.5f
      BlockboxPlayerModel.render(player,Vec3(0,0,0),PlayerAppearance.shirts(i),(1f,1f,1f))
    BlockboxPlayerModel.endLighting()
    capture(output.resolve("players.png"),width,height)
    noError("articulated players")
    // Three-quarter close-ups show the skin detail: face, hair volume, clothes and hands.
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_PROJECTION); glLoadIdentity(); glOrtho(-1.6,1.6,0.88,3.02,0.1,20)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity(); glRotatef(12,1,0,0)
    BlockboxPlayerModel.beginLighting(viewSpace = false)
    for (heading,x,i) <- Seq((214f,-0.75f,0),(28f,0.75f,1)) do
      val player = RemotePlayer("Preview",Vec3(x,1.62f,-5),Vec3(0,0,0),heading,heading,0,0,0,0)
      player.bodyYaw=heading
      BlockboxPlayerModel.render(player,Vec3(0,0,0),PlayerAppearance.shirts(i),(1f,1f,1f))
    BlockboxPlayerModel.endLighting()
    capture(output.resolve("players-closeup.png"),width,height)
    noError("player close-ups")
    val skin=BlockboxSkin.pixels(PlayerAppearance.shirts(0)); val zoom=4
    val sheet=BufferedImage(BlockboxSkin.Size*zoom,BlockboxSkin.Size*zoom,BufferedImage.TYPE_INT_RGB)
    for y <- 0 until BlockboxSkin.Size*zoom; x <- 0 until BlockboxSkin.Size*zoom do
      val p=skin((y/zoom)*BlockboxSkin.Size+x/zoom)
      val checker=if ((x/16)+(y/16))%2==0 then 0x2a3036 else 0x343b42
      sheet.setRGB(x,y,if (p>>>24)<128 then checker else p&0xffffff)
    ImageIO.write(sheet,"png",output.resolve("skin.png").toFile)

  private def firstPersonFrames(game: Blockbox,output: Path,width: Int,surfaceHeight: Int): Unit =
    // Framed at 16:9 like vanilla screenshots; tools run off the right edge on narrower screens.
    val height=width*9/16
    set(game,"framebufferHeight",height)
    try firstPersonPoses(game,output,width,height)
    finally set(game,"framebufferHeight",surfaceHeight)

  private def firstPersonPoses(game: Blockbox,output: Path,width: Int,height: Int): Unit =
    val held=game.getClass.getDeclaredMethod("renderFirstPersonHeldBlock"); held.setAccessible(true)
    val hotbar=get[Array[Block]](game,"hotbarBlocks")
    set(game,"selectedBlock",0); set(game,"viewmodelKick",0f); set(game,"worldClock",150.0)
    set(game,"antiAliasing",true)
    for (block,name,kick,obstruction) <- Seq((Block.Air,"first-person-empty",0f,0f),(Block.Torch,"first-person-torch",0f,0f),
      (Block.Stone,"first-person-block",0f,0f),(Block.Air,"first-person-mining",0.60f,0f),(Block.Stone,"first-person-near-wall",0.60f,1f),
      (Block.Torch,"first-person-torch-mining",0.60f,0f),(Block.IronPickaxe,"first-person-pickaxe",0f,0f),
      (Block.DiamondSword,"first-person-sword",0f,0f),(Block.StoneAxe,"first-person-axe",0f,0f),
      (Block.IronShovel,"first-person-shovel",0.6f,0f)) do
      hotbar(0)=block
      set(game,"viewmodelKick",kick)
      set(game,"viewmodelObstruction",obstruction)
      glUseProgram(0); glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
      glClearColor(0.12f,0.18f,0.20f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
      held.invoke(game)
      val pixels=BufferUtils.createByteBuffer(width*height*4)
      glReadPixels(0,0,width,height,GL_RGBA,GL_UNSIGNED_BYTE,pixels)
      val changed=(0 until width*height).count { i =>
        math.abs((pixels.get(i*4)&255)-31)+math.abs((pixels.get(i*4+1)&255)-46)+math.abs((pixels.get(i*4+2)&255)-51)>12
      }
      assert(changed>2000,s"$name must draw a visible hand")
      // Inside the multisampled pass (right three quarters) but clear of every pose.
      assert((height/2 until height).forall { y => (width/4 until width*3/8).forall { x =>
        val j=(y*width+x)*4
        math.abs((pixels.get(j)&255)-31)+math.abs((pixels.get(j+1)&255)-46)+math.abs((pixels.get(j+2)&255)-51)<5
      }},"transparent hand resolve preserves the scene behind it")
      capture(output.resolve(name+".png"),width,height)
      noError(name)

  private def survivalFrames(game: Blockbox,atlas: TextureAtlas,pipeline: BlockboxShaderPipeline,
                             chunks: mutable.AnyRefMap[(Int,Int),Chunk],output: Path,width: Int,height: Int): Unit =
    chunks.values.foreach(_.dispose()); chunks.clear()
    val gen=TerrainGenerator(42)
    set(game,"terrainGen",gen); set(game,"worldSeed",42L); set(game,"worldClock",150.0)
    for cx <- -2 to 2; cz <- -2 to 2 do
      val blocks=new Array[Byte](16*16*Terrain.worldHeight)
      for x <- 0 until 16; z <- 0 until 16 do
        val wx=cx*16+x; val wz=cz*16+z
        val pond=wx>=2 && wx<=18 && wz>= -10 && wz<=5
        for y <- 0 to 39 do
          val block=if pond && y>=36 then Block.Water else if pond && y==35 then Block.Sand else if y==39 then Block.Grass else if y>=37 then Block.Dirt else Block.Stone
          blocks((y*16+z)*16+x)=block.id
      chunks((cx,cz))=Chunk(cx,cz,atlas,gen,blocks)
    def put(x: Int,y: Int,z: Int,block: Block): Unit =
      chunks((Math.floorDiv(x,16),Math.floorDiv(z,16))).setBlock(Math.floorMod(x,16),y,Math.floorMod(z,16),block)
    // Edited water is preserved above sea level by the legacy-water repair.
    for x <- 2 to 18; z <- -10 to 5; y <- 36 to 39 do put(x,y,z,Block.Water)
    for (x,z) <- Seq((-1,-1),(-10,-7),(-5,-18),(24,-3),(20,-18),(-16,5),(-9,15),(25,16)) do
      for y <- 40 to 45 do put(x,y,z,Block.Wood)
      for dx <- -2 to 2; dz <- -2 to 2; y <- 44 to 47 if !(math.abs(dx)==2 && math.abs(dz)==2) && (dx!=0 || dz!=0 || y>45) do put(x+dx,y,z+dz,Block.Leaves)
    put(-2,40,4,Block.Furnace)
    chunks.values.foreach { c =>
      for dx <- -1 to 1; dz <- -1 to 1 if dx!=0 || dz!=0 do chunks.get((c.cx+dx,c.cz+dz)).foreach(c.setNeighbor(dx,dz,_))
    }
    chunks.values.foreach(c => assert(c.forceBuildNowAndUpload()))
    // Inspect the uploaded mesh: bank corners must stay fixed while interior water
    // can move. These are the actual VBO weights consumed by both HDR packs.
    val pondChunk=chunks((0,0))
    val waterVbo=get[Int](pondChunk,"waterVbo"); val waterCount=get[Int](pondChunk,"waterCount")
    val waterVertices=BufferUtils.createFloatBuffer(waterCount*15)
    org.lwjgl.opengl.GL15.glBindBuffer(org.lwjgl.opengl.GL15.GL_ARRAY_BUFFER,waterVbo)
    org.lwjgl.opengl.GL15.glGetBufferSubData(org.lwjgl.opengl.GL15.GL_ARRAY_BUFFER,0L,waterVertices)
    org.lwjgl.opengl.GL15.glBindBuffer(org.lwjgl.opengl.GL15.GL_ARRAY_BUFFER,0)
    var bankCorners=0; var interiorCorners=0
    for i <- 0 until waterCount if waterVertices.get(i*15+8)>0.5f do
      val x=waterVertices.get(i*15); val z=waterVertices.get(i*15+2); val weight=waterVertices.get(i*15+14)
      if x==2f || z==6f then
        assert(weight==0f,"shoreline vertices cannot rise above the bank"); bankCorners+=1
      if x>=5f && x<=13f && z>= -6f && z<=2f then
        assert(weight==1f,"interior water retains its waves"); interiorCorners+=1
    assert(bankCorners>0 && interiorCorners>0,"the shoreline fixture must exercise both wave states")
    val floatingBlocks=new Array[Byte](16*16*Terrain.worldHeight)
    floatingBlocks((10*16+8)*16+8)=Block.Water.id
    val floating=Chunk(5,5,atlas,gen,floatingBlocks)
    try
      assert(floating.getWaterRawLevel(8,10,8)==0,"fixture uses static water")
      assert(floating.forceBuildNowAndUpload())
      assert(get[Int](floating,"waterCount")==36,"an isolated water volume closes its top, bottom and four sides")
    finally floating.dispose()
    val remotes=get[mutable.HashMap[String,RemotePlayer]](game,"remotePlayers")
    val mobs=get[ArrayBuffer[ZombieSystem.Zombie]](game,"zombies")
    val hotbar=get[Array[Block]](game,"hotbarBlocks"); val counts=get[Array[Int]](game,"hotbarCounts")
    hotbar(0)=Block.IronPickaxe; counts(0)=1; hotbar(1)=Block.IronSword; counts(1)=1
    set(game,"selectedBlock",0); set(game,"viewmodelKick",0f); set(game,"viewmodelObstruction",0f)
    val world=game.getClass.getDeclaredMethod("renderWorld"); world.setAccessible(true)
    val hand=game.getClass.getDeclaredMethod("renderFirstPersonHeldBlock"); hand.setAccessible(true)
    val water=game.getClass.getDeclaredMethod("renderUnderwaterOverlay"); water.setAccessible(true)
    val health=game.getClass.getDeclaredMethod("renderHealth"); health.setAccessible(true)
    val bar=game.getClass.getDeclaredMethod("renderHotbar"); bar.setAccessible(true)
    set(game,"shadersEnabled",true); set(game,"foveatedRendering",false); set(game,"cloudsEnabled",true)
    assert(pipeline.selectPack("ultra"))
    def frame(name: String,eye: Vec3,heading: Float,look: Float,rain: Float,under: Boolean=false,shaders: Boolean=true): Unit =
      set(game,"camera",eye); set(game,"yaw",heading); set(game,"pitch",look)
      set(game,"viewmodelYaw",heading); set(game,"viewmodelPitch",look)
      set(game,"shadersEnabled",shaders)
      set(game,"localRain",rain); set(game,"localCloud",rain); set(game,"stormCloudCover",rain)
      set(game,"localWetness",rain); set(game,"weatherWind",0.5f)
      set(game,"weatherCells",Vector(WeatherField.Cell(0,0,260f,rain,42)))
      remotes.values.foreach(_.lastSeen=org.lwjgl.glfw.GLFW.glfwGetTime())
      glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
      if shaders then
        assert(pipeline.prepareFrame(width,height)); pipeline.clearPreparedFrame(0.22f,0.27f,0.34f)
      else {glClearColor(0.22f,0.27f,0.34f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)}
      if under then
        val submerged=game.getClass.getDeclaredMethod("isUnderwater"); submerged.setAccessible(true)
        assert(submerged.invoke(game)==java.lang.Boolean.TRUE,"the underwater review must contain actual water at eye level")
      world.invoke(game)
      if shaders then pipeline.finishFrame(width,height,150)
      hand.invoke(game)
      if under then water.invoke(game)
      BlockboxRender2D.setupOrtho(width,height); glDisable(GL_DEPTH_TEST)
      health.invoke(game); bar.invoke(game)
      capture(output.resolve(name+".png"),width,height); noError(name)
    val dry=RemotePlayer("Iron explorer",Vec3(-3,41.62f,0),Vec3(-3,41.62f,0),145,145,0,0,0,0,Block.IronPickaxe)
    remotes(dry.name)=dry
    val wet=RemotePlayer("Swimming",Vec3(8,40.4f,-2),Vec3(8,40.4f,-2),150,150,0,0,0,1,Block.DiamondSword)
    remotes(wet.name)=wet
    frame("survival-clear",Vec3(0,43,14),0,13,0)
    // A walking pose may move the held tool, but cannot rotate the atmosphere,
    // terrain, water reflection or clouds. Compare the actual rendered pixels.
    def pixels(): Array[Int] =
      val b=BufferUtils.createByteBuffer(width*height*4)
      glReadPixels(0,0,width,height,GL_RGBA,GL_UNSIGNED_BYTE,b)
      Array.tabulate(width*height*4)(i => b.get(i)&255)
    val resting=pixels()
    set(game,"cameraBobPhase",1.2f); set(game,"cameraBobAmount",1f); set(game,"cameraRoll",0.3f)
    frame("survival-walking-bob",Vec3(0,43,14),0,13,0)
    val walking=pixels()
    val skyChanges=(height*3/4 until height).flatMap(y => (0 until width).map(x => (y*width+x)*4))
      .count(i => (0 until 3).exists(c => math.abs(resting(i+c)-walking(i+c))>1))
    val handChanges=(height/8 until height/2).flatMap(y => (width*3/4 until width).map(x => (y*width+x)*4))
      .count(i => (0 until 3).exists(c => math.abs(resting(i+c)-walking(i+c))>4))
    assert(skyChanges==0,s"walking bob leaves the sky/cloud camera stable: $skyChanges changed pixels")
    assert(handChanges>20,s"the held tool still bobs when walking: $handChanges changed pixels")
    set(game,"cameraBobPhase",0f); set(game,"cameraBobAmount",0f); set(game,"cameraRoll",0f)
    val originalPosition=dry.pos
    dry.pos=Vec3(-4,41.62f,8)
    frame("survival-shadow-with-avatar",Vec3(0,43,14),0,13,0)
    val project=game.getClass.getDeclaredMethod("projectWorldToScreen",classOf[Vec3]); project.setAccessible(true)
    val foot=project.invoke(game,Vec3(-4,40,8)).asInstanceOf[Option[(Float,Float,Float)]].get
    val sx=foot._1.toInt-70; val sy=height-foot._2.toInt-45
    def groundPixels(): Array[Float] =
      val pixels=BufferUtils.createFloatBuffer(100*40*4)
      glReadPixels(sx,sy,100,40,GL_RGBA,GL_FLOAT,pixels)
      Array.tabulate(100*40)(i => pixels.get(i*4)*0.2126f+pixels.get(i*4+1)*0.7152f+pixels.get(i*4+2)*0.0722f)
    val shaded=groundPixels()
    remotes.remove(dry.name)
    frame("survival-shadow-without-avatar",Vec3(0,43,14),0,13,0)
    val bare=groundPixels()
    val shadowPixels=bare.zip(shaded).count((a,b) => a-b>0.012f)
    assert(shadowPixels>=8,s"an avatar casts a visible shadow on the ground: $shadowPixels pixels")
    remotes(dry.name)=dry
    frame("survival-entity-lighting",Vec3(-4,41.8f,12),0,4,0)
    dry.pos=originalPosition
    frame("survival-storm",Vec3(0,43,14),0,13,1)
    for pack <- Seq("ultra","ray-traced","default") do
      assert(pipeline.selectPack(pack)); frame("survival-storm-"+pack,Vec3(0,43,14),0,13,1)
      if pack=="ray-traced" then
        if sys.env.get("BLOCKBOX_REQUIRE_HARDWARE_RT").contains("1") then assert(BlockboxRayTrace.frameReady,BlockboxRayTrace.status)
        println("[ray-trace] "+BlockboxRayTrace.status+"; completed world frame="+BlockboxRayTrace.frameReady)
    assert(pipeline.selectPack("ultra"))
    get[AirSupply](game,"airSupply").remaining=7f
    frame("survival-underwater",Vec3(8,38.2f,4),0,3,0,true)
    frame("survival-underwater-compatibility",Vec3(8,38.2f,4),0,3,0,true,false)
    for pack <- Seq("ultra","ray-traced","default") do
      assert(pipeline.selectPack(pack))
      frame("survival-underwater-up-"+pack,Vec3(8,38.2f,4),-65,-55,0,true)
      if pack!="default" then
        def edgeSkyVisible(): Unit =
          val patch=BufferUtils.createFloatBuffer(48*48*4)
          glReadPixels(width/30,height/2-24,48,48,GL_RGBA,GL_FLOAT,patch)
          val luminance=(0 until 48*48).map(i => patch.get(i*4)*0.2126f+patch.get(i*4+1)*0.7152f+patch.get(i*4+2)*0.0722f).sum/(48*48)
          assert(luminance>0.18f,s"$pack keeps the high-angle sky visible through the underwater interface: $luminance")
        edgeSkyVisible()
        val canopy=project.invoke(game,Vec3(-1,46,-1)).asInstanceOf[Option[(Float,Float,Float)]].get
        val leafPixels=BufferUtils.createFloatBuffer(100*100*4)
        glReadPixels(canopy._1.toInt-50,height-canopy._2.toInt-50,100,100,GL_RGBA,GL_FLOAT,leafPixels)
        val green=(0 until 100*100).count { i =>
          val r=leafPixels.get(i*4); val g=leafPixels.get(i*4+1); val b=leafPixels.get(i*4+2)
          g>r*1.3f && g>b*0.8f && g>0.06f
        }
        assert(green>40,s"$pack preserves above-water foliage seen from below: $green pixels")
        frame("survival-underwater-near-surface-"+pack,Vec3(8,39.94f,4),-65,-55,0,true)
        edgeSkyVisible()
    frame("survival-underwater-up-compatibility",Vec3(8,38.2f,4),-65,-55,0,true,false)
    assert(pipeline.selectPack("ultra"))
    get[AirSupply](game,"airSupply").reset()
    val zombie=ZombieSystem.Zombie(1,Vec3(-2,40,-2)); zombie.yaw=160; zombie.gait=0.8f; zombie.stride=0.5f
    mobs+=zombie
    set(game,"worldClock",850.0)
    frame("survival-zombie-night",Vec3(0,41.8f,6),0,1,0)
    zombie.hurt=1f; dry.hurt=1f
    frame("survival-damage-flash",Vec3(0,41.8f,6),0,1,0)
    zombie.hurt=0f; dry.hurt=0f
    set(game,"worldClock",150.0); zombie.burning=4f
    frame("survival-zombie-burning",Vec3(0,41.8f,6),0,1,0)
    zombie.burning=0f; zombie.pos=Vec3(8,38.75f,-2); zombie.swimming=1f; zombie.bodyYaw=zombie.yaw
    for i <- 0 until 4 do
      zombie.gait=i*math.Pi.toFloat*0.5f; zombie.stride=0.7f
      frame(s"survival-zombie-swimming-$i",Vec3(8,41.2f,5),0,8,0)
    remotes.clear(); mobs.clear()
    // Review the same extruded silhouettes in the UI at native pixel scale.
    glBindFramebuffer(GL_FRAMEBUFFER,0); glClearColor(0.08f,0.11f,0.13f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
    BlockboxRender2D.setupOrtho(width,height); glDisable(GL_DEPTH_TEST)
    for ((_,items),tier) <- ToolSystem.materials.zipWithIndex; (item,i) <- items.zipWithIndex do
      ItemModel.icon(item,100f+i*180f,40f+tier*140f,100)
    capture(output.resolve("survival-tool-catalog.png"),width,height); noError("solid tools and UI icons")

  private def inventoryFrames(game: Blockbox,output: Path,width: Int,height: Int): Unit =
    val hotbar=get[Array[Block]](game,"hotbarBlocks"); val hotCounts=get[Array[Int]](game,"hotbarCounts")
    val storage=get[Array[Block]](game,"backpackBlocks"); val counts=get[Array[Int]](game,"backpackCounts")
    val crafting=get[Array[Block]](game,"craftGridBlocks"); val craftCounts=get[Array[Int]](game,"craftGridCounts")
    Seq(Block.IronSword,Block.IronPickaxe,Block.StoneAxe,Block.IronShovel,Block.Torch,Block.Planks,Block.Stone,Block.CoalLump,Block.IronIngot).zipWithIndex.foreach { (b,i) =>
      hotbar(i)=b; hotCounts(i)=if ToolSystem.isTool(b) then 1 else 24
    }
    Seq(Block.Stone,Block.Wood,Block.Sand,Block.DiamondGem,Block.GoldOre,Block.IronOre,Block.Copper,Block.Leaves,Block.Stick).zipWithIndex.foreach { (b,i) => storage(i)=b; counts(i)=64-i*5 }
    for i <- 0 until 3 do {crafting(i)=Block.IronIngot; craftCounts(i)=3}
    for i <- Seq(4,7) do {crafting(i)=Block.Stick; craftCounts(i)=3}
    set(game,"selectedBlock",0); set(game,"heldInventoryBlock",Block.Air); set(game,"gameMode",GameMode.Survival)
    val inventory=game.getClass.getDeclaredMethod("renderSurvivalInventory"); inventory.setAccessible(true)
    val catalog=game.getClass.getDeclaredMethod("renderCatalog"); catalog.setAccessible(true)
    for (w,h,name,isCatalog,category,query) <- Seq((width,height,"inventory",false,0,""),
      (640,360,"inventory-compact",false,0,""),(width,height,"catalog",true,0,""),
      (width,height,"catalog-tools",true,2,""),(width,height,"catalog-search",true,0,"diamond")) do
      glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,w,h); glUseProgram(0)
      glClearColor(0.18f,0.26f,0.20f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
      set(game,"framebufferWidth",w); set(game,"framebufferHeight",h)
      set(game,"catalogCategory",category); set(game,"catalogQuery",query)
      if isCatalog then catalog.invoke(game) else inventory.invoke(game)
      capture(output.resolve(s"survival-$name.png"),w,h); noError(name)
    set(game,"framebufferWidth",width); set(game,"framebufferHeight",height)
    glViewport(0,0,width,height)
    val player=game.getClass.getDeclaredMethod("renderInventoryPlayer",classOf[UiRect],java.lang.Float.TYPE,java.lang.Float.TYPE)
    val scale=game.getClass.getDeclaredMethods.find(m => m.getName.endsWith("uiScale") && m.getParameterCount==0).get
    player.setAccessible(true); scale.setAccessible(true)
    val preview=BlockboxUiLayout.inventory(width,height,scale.invoke(game).asInstanceOf[Float]).preview
    for (name,mx,my) <- Seq(("right",preview.x+preview.w*2.5f,preview.y+preview.h*0.19f),
      ("up-left",preview.x-preview.w*1.5f,preview.y-preview.h*0.6f)) do
      glClearColor(0.18f,0.26f,0.20f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
      player.invoke(game,preview,Float.box(mx),Float.box(my))
      capture(output.resolve(s"inventory-look-$name.png"),width,height); noError("inventory look "+name)

  private def heldToolFrames(game: Blockbox,output: Path,width: Int,height: Int): Unit =
    val grip=game.getClass.getDeclaredMethod("drawHeldItemAtGrip",classOf[Block],classOf[Tuple3[?,?,?]])
    grip.setAccessible(true)
    for (name,heading) <- Seq(("front",180f),("three-quarter",220f),("side",270f)) do
      glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
      glClearColor(0.12f,0.18f,0.20f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
      BlockboxRender2D.resetGlArraysAndBuffers(); glUseProgram(0)
      glEnable(GL_DEPTH_TEST); glDepthMask(true); glDisable(GL_BLEND); glDisable(GL_FOG); glDisable(GL_CULL_FACE)
      glMatrixMode(GL_PROJECTION); glLoadIdentity(); glOrtho(-3.1,3.1,-0.10,2.85,0.1,20)
      glMatrixMode(GL_MODELVIEW); glLoadIdentity(); glRotatef(8f,1,0,0)
      BlockboxPlayerModel.beginLighting(viewSpace=false)
      for (item,i) <- Seq(Block.DiamondSword,Block.IronPickaxe,Block.StoneAxe,Block.IronShovel).zipWithIndex do
        val player=RemotePlayer("Held tool",Vec3(-2.3f+i*1.55f,1.62f,-5),Vec3(0,0,0),heading,heading,0,0,0,i,item)
        val held=Some(() => {grip.invoke(game,item,(1f,1f,1f)); ()})
        BlockboxPlayerModel.render(player,Vec3(0,0,0),PlayerAppearance.shirts(i),(1f,1f,1f),0f,held)
      BlockboxPlayerModel.endLighting()
      capture(output.resolve("third-person-tools-"+name+".png"),width,height); noError("held tool "+name)

  def atmosphereFrames(output: Path,width: Int,height: Int): Unit =
    glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
    BlockboxRender2D.resetGlArraysAndBuffers(); glUseProgram(0)
    glDisable(GL_DEPTH_TEST); glDepthMask(false); glDisable(GL_FOG); glDisable(GL_TEXTURE_2D)
    val halfY=math.tan(math.toRadians(70)/2).toFloat; val halfX=halfY*width/height
    glMatrixMode(GL_PROJECTION); glLoadIdentity(); glFrustum(-halfX*0.25,halfX*0.25,-halfY*0.25,halfY*0.25,0.25,1000)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    for (name,time,rain,flash) <- Seq(("sky-day",150f,0f,0f),("sky-dawn",8f,0f,0f),("sky-sunset",590f,0f,0f),
      ("sky-night",850f,0f,0f),("sky-storm",150f,0.9f,0f),("sky-lightning",850f,0.9f,1f)) do
      val phase=BlockboxSky.dayPhase(time)
      val palette=BlockboxAtmosphere.palette(phase,BlockboxSky.daylightFactor(phase),rain,rain,flash)
      glClearColor(0,0,0,1); glClear(GL_COLOR_BUFFER_BIT)
      assert(BlockboxAtmosphere.render(0,0,palette,true),"analytic sky shader compiled")
      val pixels=BufferUtils.createFloatBuffer(width*height*4)
      glReadPixels(0,0,width,height,GL_RGBA,GL_FLOAT,pixels)
      for x <- Seq(width/4,width/2,3*width/4); y <- Seq(height/8,height/2,7*height/8) do
        val direction=Vec3(((x+0.5f)/width*2-1)*halfX,((y+0.5f)/height*2-1)*halfY,-1)
        val expected=BlockboxAtmosphere.color(direction,palette)
        val i=(y*width+x)*4
        assert(Seq(expected.x,expected.y,expected.z).zipWithIndex.forall((v,c) => math.abs(v-pixels.get(i+c))<0.006f),s"$name matches continuous atmosphere reference")
      val rows=(height/2 until height*7/8).map(y => pixels.get((y*width+width/2)*4))
      val biggestStep=rows.sliding(2).map(p => math.abs(p(1)-p(0))).max
      assert(biggestStep<0.015f,s"$name has no visible ring boundary")
      capture(output.resolve(name+".png"),width,height); noError(name)
    glDepthMask(true)

  private def torchFrames(game: Blockbox,atlas: TextureAtlas,pipeline: BlockboxShaderPipeline,
                          chunks: mutable.AnyRefMap[(Int,Int),Chunk],output: Path,width: Int,height: Int): Unit =
    chunks.values.foreach(_.dispose()); chunks.clear()
    val gen=TerrainGenerator(42)
    for cx <- 0 to 1 do chunks((cx,0))=Chunk(cx,0,atlas,gen,new Array[Byte](16*16*Terrain.worldHeight))
    def put(x: Int,y: Int,z: Int,block: Block,mount: TorchMount=TorchMount.Floor): Unit =
      chunks((Math.floorDiv(x,16),0)).setBlock(Math.floorMod(x,16),y,z,block,LogAxis.Y,mount)
    for x <- 0 until 32; z <- 0 until 16 do put(x,39,z,Block.Stone)
    for mount <- TorchMount.values do
      val x=9+mount.id*3; val y=40; val z=6
      val (sx,sy,sz)=mount.support
      put(x+sx,y+sy,z+sz,Block.Brick)
      put(x,y,z,Block.Torch,mount)
    chunks((0,0)).setNeighbor(1,0,chunks((1,0))); chunks((1,0)).setNeighbor(-1,0,chunks((0,0)))
    chunks.values.foreach(c => assert(c.forceBuildNowAndUpload(),"all torch vertices respect the chunk VBO layout"))
    val render=game.getClass.getDeclaredMethod("renderWorld"); render.setAccessible(true)
    set(game,"shadersEnabled",true); set(game,"foveatedRendering",false)
    set(game,"cloudsEnabled",false); set(game,"camera",Vec3(15,43,14)); set(game,"yaw",0f); set(game,"pitch",18f)
    val refreshFlames=game.getClass.getDeclaredMethod("refreshTorchFlames"); refreshFlames.setAccessible(true)
    refreshFlames.invoke(game)
    assert(TorchEffects.flameCount==TorchMount.values.length,"every placed torch has a live flame")
    for _ <- 0 until 80 do TorchEffects.update(0.05f,Vec3(15,43,14))
    assert(TorchEffects.particleCount>TorchMount.values.length,"torches emit flame wisps and smoke")
    def frame(name: String,time: Double,shaders: Boolean): Unit =
      set(game,"worldClock",time); set(game,"shadersEnabled",shaders)
      glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
      if shaders then
        assert(pipeline.prepareFrame(width,height)); pipeline.clearPreparedFrame(0.1f,0.15f,0.2f)
      else { glClearColor(0.1f,0.15f,0.2f,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT) }
      render.invoke(game)
      if shaders then pipeline.finishFrame(width,height,time.toFloat)
      capture(output.resolve(name+".png"),width,height); noError(name)
    frame("torches-day",150,true)
    frame("torches-night",900,true)
    def floorBrightness(): Float =
      val w=width/2; val h=height/4
      val pixels=BufferUtils.createFloatBuffer(w*h*4)
      glReadPixels(width/4,height/4,w,h,GL_RGBA,GL_FLOAT,pixels)
      (0 until w*h).map(i => pixels.get(i*4)*0.3f+pixels.get(i*4+1)*0.6f+pixels.get(i*4+2)*0.1f).sum/(w*h)
    val litBrightness=floorBrightness()
    frame("torches-compatibility-night",900,false)
    // Remove the floor below a wall torch: its own saved wall still supports it.
    val setter=game.getClass.getDeclaredMethods.find(_.getName=="setActiveBlock").get
    setter.setAccessible(true)
    setter.invoke(game,Int.box(12),Int.box(39),Int.box(6),Block.Air,LogAxis.Y,TorchMount.Floor,None)
    assert(chunks((0,0)).getBlock(12,40,6)==Block.Torch)
    // Removing its wall must remove the torch, including across a chunk seam.
    setter.invoke(game,Int.box(13),Int.box(40),Int.box(6),Block.Air,LogAxis.Y,TorchMount.Floor,None)
    assert(chunks((0,0)).getBlock(12,40,6)==Block.Air,"wall torch detaches when its saved support breaks")
    val cross=chunks((0,0)); val east=chunks((1,0))
    cross.setBlock(15,42,10,Block.Torch,LogAxis.Y,TorchMount.EastWall); east.setBlock(0,42,10,Block.Stone)
    setter.invoke(game,Int.box(16),Int.box(42),Int.box(10),Block.Air,LogAxis.Y,TorchMount.Floor,None)
    assert(cross.getBlock(15,42,10)==Block.Air,"support removal crosses the chunk boundary")
    // Water removes a torch and its light source; source caches must follow both edits.
    val flow=game.getClass.getDeclaredMethod("waterCanEnter",classOf[Block]); flow.setAccessible(true)
    assert(flow.invoke(game,Block.Torch)==java.lang.Boolean.TRUE)
    setter.invoke(game,Int.box(9),Int.box(40),Int.box(6),Block.Water,LogAxis.Y,TorchMount.Floor,None)
    assert(!cross.lightSourcesSnapshot.exists(s => s.x==9 && s.y==40 && s.z==6),"water extinguishes the light source")
    for mount <- TorchMount.values do put(9+mount.id*3,40,6,Block.Air)
    chunks.values.foreach(c => assert(c.forceBuildNowAndUpload()))
    TorchEffects.clear(); refreshFlames.invoke(game)
    assert(TorchEffects.flameCount==0,"removed torches stop glowing")
    frame("torches-unlit-night",900,true)
    assert(litBrightness>floorBrightness()+0.012f,"torchlight remains visible at night and disappears when torches are removed")

  def skyPreview(output: Path,width: Int,height: Int): Unit =
    val camera = Vec3(160f,130f,160f)
    val phase = BlockboxSky.dayPhase(150f); val daylight = BlockboxSky.daylightFactor(phase)
    val color = BlockboxSky.clearColor(false,phase,daylight,0,0.25f,0)
    glBindFramebuffer(GL_FRAMEBUFFER,0); glViewport(0,0,width,height)
    glClearColor(color._1,color._2,color._3,1); glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_PROJECTION); glLoadIdentity()
    val near = 0.25; val top = math.tan(math.toRadians(70)/2)*near
    glFrustum(-top*width/height,top*width/height,-top,top,near,1024)
    glMatrixMode(GL_MODELVIEW); glLoadIdentity()
    glRotatef(-20,1,0,0); glTranslatef(0,-camera.y,0)
    val fog = BufferUtils.createFloatBuffer(4).put(Array(color._1,color._2,color._3,1f)); fog.flip()
    glFogfv(GL_FOG_COLOR,fog)
    BlockboxSky.renderSky(0,-20,phase,daylight,BlockboxSky.generateStars(),true,camera,150,512,0,0.25f,0)
    BlockboxSky.renderClouds(camera,150,512,phase,daylight,0.25f,0,8675309L)
    capture(output.resolve("clouds.png"),width,height)
    noError("sky and cached clouds")
    BlockboxSky.destroy()

  /** Writes the block atlas, transparency shown over a checkerboard, for texture review. */
  private def exportAtlas(atlas: TextureAtlas,path: Path): Unit =
    atlas.bind()
    val w=glGetTexLevelParameteri(GL_TEXTURE_2D,0,GL_TEXTURE_WIDTH); val h=glGetTexLevelParameteri(GL_TEXTURE_2D,0,GL_TEXTURE_HEIGHT)
    val pixels=BufferUtils.createByteBuffer(w*h*4)
    glGetTexImage(GL_TEXTURE_2D,0,GL_RGBA,GL_UNSIGNED_BYTE,pixels)
    glBindTexture(GL_TEXTURE_2D,0)
    val image=BufferedImage(w,h,BufferedImage.TYPE_INT_RGB)
    for y <- 0 until h; x <- 0 until w do
      val i=(y*w+x)*4; val a=(pixels.get(i+3)&255)/255f
      val bg=if (x/4+y/4)%2==0 then 52f else 84f
      def ch(k: Int)=((pixels.get(i+k)&255)*a+bg*(1f-a)).toInt
      image.setRGB(x,y,(ch(0)<<16)|(ch(1)<<8)|ch(2))
    ImageIO.write(image,"png",path.toFile)
    noError("atlas export")

  private def capture(path: Path,width: Int,height: Int): Unit =
    assert(glGetInteger(GL_FRAMEBUFFER_BINDING)==0,s"capture must read the completed screen: $path")
    glReadBuffer(GL_BACK)
    glFinish()
    val pixels = BufferUtils.createByteBuffer(width*height*4)
    glReadPixels(0,0,width,height,GL_RGBA,GL_UNSIGNED_BYTE,pixels)
    val visible=(0 until width*height).exists(i => (0 until 3).exists(c => (pixels.get(i*4+c)&255)>8))
    assert(visible,s"rendered frame is blank: $path")
    val image = BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
    for y <- 0 until height; x <- 0 until width do
      val i = ((height-y-1)*width+x)*4
      image.setRGB(x,y,((pixels.get(i)&255)<<16)|((pixels.get(i+1)&255)<<8)|(pixels.get(i+2)&255))
    ImageIO.write(image,"png",path.toFile)
