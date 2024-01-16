package net.maryknollrad.ctdose

object DRLVals:
    private type DRLVal = Tuple3[String, Double, Double] | Tuple5[String, Double, Double, Int, Int]

    private val koreaSpec: Seq[DRLVal] = Seq(
        ("HEAD", 23.5, 429.28, 0, 2),
        ("HEAD", 31.4, 585.0, 2, 6),
        ("HEAD", 38.46, 756.0, 6, 11),
        ("HEAD", 51.49, 967.16, 11, 15),
        ("HEAD", 52.18, 969.8, 16, Int.MaxValue),
        ("HEAD - Angio", 26.99, 885.92),
        ("NECK", 13.39, 597.12),
        ("CHEST - Low Dose", 2.68, 109.48),
        ("SPINE - Cervical", 20.93, 508.66),
        ("SPINE - Thoracic", 7.6, 324.2),
        ("SPINE - Lumbar", 20.59, 738.5),
        ("HEART - Calcium Scoring", 5.9, 95.73),
        ("HEART - Conary Angio", 19.22, 326.94),
        ("ABDOMEN/PELVIS - Enhanced", 8.94, 473.7),
        ("ABDOMEN/PELVIS - Nonenhanced", 10.28, 558.4),
        ("ABDOMEN - Dynamic P1", 8.66, 345.35),
        ("ABDOMEN - Dynamic P2", 8.66 + 9.53, 345.35 + 326.69),
        ("ABDOMEN - Dynamic P3", 8.66 + 9.53 + 9.42, 345.35 + 326.69 + 457.0),
        ("ABDOMEN - Dynamic P4", 8.66 + 9.53 + 9.42 + 9.46, 345.35 + 326.69 + 457.0 + 401.4))

    private val euclidSpec: Seq[DRLVal] = Seq(
        ("STROKE", 48, 1386),
        ("SINUSITIS", 11, 211),
        ("CERVICAL SPINE", 17, 495),
        ("PULMONARY EMBOLISM", 9, 364),
        ("CORONARY - Calcium Scoring", 4, 81),
        ("CORONARY - Angiography", 25, 459),
        ("LUNG CANCER", 8, 628),
        ("HCC", 9, 1273),
        ("ABDOMINAL PAIN", 8, 480),
        ("APPENDICITIS", 9, 874))

    private val japanSpec: Seq[DRLVal] = Seq(
        ("Routine Brain", 30, 480, 0, 1), 
        ("Routine Brain", 40, 600, 1, 5), 
        ("Routine Brain", 55, 850, 5, 10), 
        ("Routine Brain", 60, 1000, 10, 15), 
        ("Routine Brain", 77, 1350, 16, Int.MaxValue), 
        ("Routine Chest", 6, 140, 0, 1), 
        ("Routine Chest", 8, 190, 1, 5), 
        ("Routine Chest", 13, 350, 5, 10), 
        ("Routine Chest", 13, 460, 10, 15), 
        ("Routine Chest", 13, 510, 16, Int.MaxValue),
        ("Chest and Pelvis", 16, 1200),
        ("Abdomen and Pelvis", 10, 220, 0, 1), 
        ("Abdomen and Pelvis", 12, 380, 1, 5), 
        ("Abdomen and Pelvis", 15, 530, 5, 10), 
        ("Abdomen and Pelvis", 18, 900, 10, 15), 
        ("Abdomen and Pelvis", 18, 880, 16, Int.MaxValue),
        ("Liver - multiphase", 17, 2100),
        ("Coronary - Angiography", 66, 1300), 
        ("PTE and DVT", 14, 2600),
        ("Whole body for trauma", 0, 5800))

    private val drls = Map("korea" -> koreaSpec, "euclid" -> euclidSpec, "japan" -> japanSpec)

    def initCategoriesIfNedded(db: DB, cs: String*) = 
        import cats.implicits.*
        
        val keys = if cs.length == 0 then drls.keys.toSeq else cs.map(_.toLowerCase).filter(drls.contains)
        keys.flatTraverse(k =>
            db.insertCategoryIfNedded(k).flatMap( _ match 
                case count if count > 0 => 
                    drls(k).zipWithIndex.traverse((t, i) => 
                        t match
                            case t3: Tuple3[String, Double, Double] => db.insertDrl(k, i+1, t3._1, t3._2, t3._3)
                            case t5: Tuple5[String, Double, Double, Int, Int] => db.insertDrl(k, i+1, t5._1, t5._2, t5._3, t5._4, t5._5)
                    )
                case _ =>
                    cats.effect.IO(Seq.empty)
            )
        )