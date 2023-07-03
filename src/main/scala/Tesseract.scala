import net.sourceforge.tess4j.*
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.FileSystems

object Tesseract:
    private val instance = new Tesseract()
    private var tesseractPath = ""

    def setTesseractPath(tp: String) = 
        val sep = FileSystems.getDefault().getSeparator()
        val jnaPath = Seq(tp, "lib").mkString(sep)
        val dataPath = Seq(tp, "share", "tessdata").mkString(sep)
        System.setProperty("jna.library.path", jnaPath)
        instance.setDatapath(dataPath)
        instance.setLanguage("eng")

    def doOCR(image: BufferedImage) = 
        instance.doOCR(image)
