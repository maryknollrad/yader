package net.maryknollrad.ctdose

import net.sourceforge.tess4j.*
import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.FileSystems

object Tesseract:
    private val instance = new Tesseract()
    private var tesseractPath = ""
    private val windows = "windows.*".r

    def setTesseractPath(tp: String) = 
        val osname = System.getProperty("os.name").toLowerCase()
        val (jnaPath, dataPath) = osname match 
            case windows() => (os.Path(tp), os.Path(tp) / "tessdata")
            case _ => (os.Path(tp) / "lib", os.Path(tp) / "share" / "tessdata")
        System.setProperty("jna.library.path", jnaPath.toString)
        instance.setDatapath(dataPath.toString)
        instance.setLanguage("eng")

    def doOCR(image: BufferedImage) = 
        instance.doOCR(image)
