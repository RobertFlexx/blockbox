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
  val width = 960; val height = 640
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
    ShaderPipelineTest.pipeline(root,width,height)
    ShaderPipelineTest.gameFrames(root,artifacts,width,height)
    ShaderPipelineTest.playerPreview(artifacts,width,height)
    ShaderPipelineTest.atmosphereFrames(artifacts,width,height)
    ShaderPipelineTest.polishFrames(root,artifacts,width,height)
    ShaderPipelineTest.skyPreview(artifacts,width,height)
    println(s"[pipeline-render] all engine render checks passed; previews in $artifacts")
  finally
    org.lwjgl.opengl.GL43.glDebugMessageCallback(null,0L)
    debug.free()
    eglMakeCurrent(display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT)
    eglDestroyContext(display,context); eglDestroySurface(display,surface); eglTerminate(display)

object ShaderPipelineTest:
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
    assert(clips.size==37)
    val rain=clips("weather/rain-loop.ogg")
    assert(math.abs(rain.head-rain.last)<0.025f,"decoded ambience has a smooth loop boundary")
    PresentationTest.writeWav(output.resolve("recorded-footsteps.wav"),Seq("grass","concrete","wood","snow").flatMap { material =>
      (0 until 3).flatMap(i => clips(f"kenney/footstep_${material}_${i}%03d.ogg").toVector++Vector.fill(10000)(0f))
    }.toArray)
    PresentationTest.writeWav(output.resolve("recorded-weather.wav"),rain.take(44100*8)++Array.fill(15000)(0f)++clips("weather/thunder-distant.ogg"))
    assert(RecordedAudio.load("../outside.ogg").isEmpty)
    val temporary=Files.createTempDirectory("blockbox-sky-polish")
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("assets/shaders"),temporary.resolve("assets/shaders"))
    blockbox.io.BlockboxFiles.copyRecursive(root.resolve("shaderpacks"),temporary.resolve("shaderpacks"))
    val pipeline=BlockboxShaderPipeline(temporary.toFile,Some("default")); pipeline.initialize()
    assert(pipeline.selectPack("aether-ultra"),"UI selection overrides the launch pack")
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
      assert(pipeline.selectPack("minecraft-plus"))
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
      println("[polish-render] 37 recordings, loop continuity, HDR sun/moon and distant storm passed")
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
      for _ <- 0 until 2; pack <- Vector("aether-ultra","default","clarity","default","minecraft-plus","default","nocturne","default","aether-ultra") do
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
    finally
      pipeline.destroy(); scale.release()
      blockbox.io.BlockboxFiles.deleteRecursive(temporary)

  private def set(game: Blockbox,name: String,value: Any): Unit =
    val field = game.getClass.getDeclaredFields.find(f => f.getName==name || f.getName.endsWith("$$"+name)).get
    field.setAccessible(true); field.set(game,value)

  private def get[A](game: Blockbox,name: String): A =
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
    Files.writeString(temporary.resolve("shaderpacks/active.txt"),"minecraft-plus\n")
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
        assert(pipeline.prepareFrame(width,height)); pipeline.clearPreparedFrame(0.60f,0.73f,0.85f)
        render.invoke(game)
        if pitch==90f then assert(get[ArrayBuffer[Chunk]](game,"renderChunkScratch").length==49,"downward camera retains all loaded chunks at every yaw")
        pipeline.finishFrame(width,height,150)
        noError(name)
        capture(output.resolve(name+".png"),width,height)
      val spawn = BlockboxWorld.findSafeSpawn(gen,(x,y,z) => chunks.get((Math.floorDiv(x,16),Math.floorDiv(z,16))).map(c => c.getBlock(Math.floorMod(x,16),y,Math.floorMod(z,16))))
      frame("terrain",spawn+Vec3(0,10f,0),0,22,true)
      for pack <- Vector("aether-ultra","default","clarity","nocturne","minecraft-plus") do
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
    finally
      chunks.values.foreach(_.dispose()); atlas.destroy(); pipeline.destroy(); fovea.release()
      get[FirstPersonPass](game,"firstPersonPass").destroy(); BlockboxSky.destroy(); TorchEffects.destroy(); BlockboxSkin.dispose()
      blockbox.io.BlockboxFiles.deleteRecursive(temporary)

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

  private def firstPersonFrames(game: Blockbox,output: Path,width: Int,height: Int): Unit =
    val held=game.getClass.getDeclaredMethod("renderFirstPersonHeldBlock"); held.setAccessible(true)
    val hotbar=get[Array[Block]](game,"hotbarBlocks")
    set(game,"selectedBlock",0); set(game,"viewmodelKick",0f); set(game,"worldClock",150.0)
    set(game,"antiAliasing",true)
    for (block,name,kick,obstruction) <- Seq((Block.Air,"first-person-empty",0f,0f),(Block.Torch,"first-person-torch",0f,0f),
      (Block.Stone,"first-person-block",0f,0f),(Block.Air,"first-person-mining",0.60f,0f),(Block.Stone,"first-person-near-wall",0.60f,1f),
      (Block.Torch,"first-person-torch-mining",0.60f,0f)) do
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
      assert((0 until width*height/8).forall { i =>
        val x=i%(width/4); val y=i/(width/4); val j=(y*width+x)*4
        math.abs((pixels.get(j)&255)-31)+math.abs((pixels.get(j+1)&255)-46)+math.abs((pixels.get(j+2)&255)-51)<5
      },"transparent hand resolve preserves the scene behind it")
      capture(output.resolve(name+".png"),width,height)
      noError(name)

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
    val pixels = BufferUtils.createByteBuffer(width*height*4)
    glReadPixels(0,0,width,height,GL_RGBA,GL_UNSIGNED_BYTE,pixels)
    val image = BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
    for y <- 0 until height; x <- 0 until width do
      val i = ((height-y-1)*width+x)*4
      image.setRGB(x,y,((pixels.get(i)&255)<<16)|((pixels.get(i+1)&255)<<8)|(pixels.get(i+2)&255))
    ImageIO.write(image,"png",path.toFile)
