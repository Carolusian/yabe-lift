package code.lib

import net.liftweb.common._
import net.liftweb.http._
import java.util.Random
import java.awt.image.BufferedImage
import java.awt.Graphics2D
import java.awt.Font
import java.awt.Color
import java.awt.GradientPaint
import java.awt.RenderingHints
import java.io._
import javax.imageio.ImageIO

object YabeHelper {
  /**
   * Control list style from posts, users, etc..
   */
  def oddOrEven(current:String) = {
    current match {
      case "odd" => "even"
      case _ => "odd"
    }
  }
  
  def fmtDateStr(date:java.util.Date) = {
    date match {
      case null => ""
      case _=>  val format = new java.text.SimpleDateFormat("yyyy-MM-dd");format.format(date)
    }
  }
  
  def captcha():Box[LiftResponse] = {
    Full(InMemoryResponse(generateCaptchaData, List("Content-Type" -> "image/png"), Nil, 200))
  }
  
  private def generateCaptchaData = {
    val width = 150
    val height = 50
    
    val data = 
      Array(Array('z', 'e', 't', 'c', 'o', 'd', 'e'),
          Array('l', 'i', 'n', 'u', 'x'),
          Array('f', 'r', 'e', 'e', 'b', 's', 'd'),
          Array('u', 'b', 'u', 'n', 't', 'u'),
          Array('j', 'e', 'e'))
    
    val bufferedImage = new BufferedImage(width, height, 
                  BufferedImage.TYPE_INT_RGB)
    
    val g2d = bufferedImage.createGraphics()
    val font = new Font("Georgia",Font.BOLD,18)
    g2d.setFont(font)
    
    val rh = new RenderingHints(
           RenderingHints.KEY_ANTIALIASING,
           RenderingHints.VALUE_ANTIALIAS_ON)

    rh.put(RenderingHints.KEY_RENDERING, 
           RenderingHints.VALUE_RENDER_QUALITY)

    g2d.setRenderingHints(rh)

    val gp = new GradientPaint(0, 0, 
    Color.red, 0, height/2, Color.black, true)

    g2d.setPaint(gp)
    g2d.fillRect(0, 0, width, height)

    g2d.setColor(new Color(255, 153, 0))

    val r = new Random()
    val index = Math.abs(r.nextInt()) % 5

    val captcha = String.copyValueOf(data(index))
    S.setSessionAttribute("captcha", captcha )

    var x = 0
    var y = 0

    for (i <- 0 until data(index).length) {
        x += 10 + (Math.abs(r.nextInt()) % 15)
        y = 20 + Math.abs(r.nextInt()) % 20
        g2d.drawChars(data(index), i, 1, x, y)
    }

    g2d.dispose()
    val baos = new ByteArrayOutputStream();
    ImageIO.write(bufferedImage, "png", baos)
    val captchaData = baos.toByteArray()
    baos.close
    
    captchaData
  }
}