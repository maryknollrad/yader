package net.maryknollrad.d4cs

import org.dcm4che3.data.Attributes

object DicomTags:
    import DicomBase.*
    def apply(tags: DicomTag*): DicomTags = Seq(tags:_*)

    def getStringTag(tag: Int, attr: Attributes, encoding: String = "utf-8"): String = 
        String(attr.getBytes(tag), encoding)

    def printTags(encoding: String = "utf-8")(tags: DicomTags, attr: Attributes): Unit = 
        inline def print(t: Int) = println(getStringTag(t, attr, encoding))
        tags.foreach: tag =>
             tag match
                case QueryTag(t, v) => 
                    println(s"(${t}) $v")
                case t: RetrieveTag =>
                    println(s"(${t}) ${getStringTag(t.asInstanceOf[Int], attr, encoding)}")

