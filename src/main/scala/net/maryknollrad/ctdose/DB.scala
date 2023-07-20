package net.maryknollrad.ctdose

import cats.effect.IO
import doobie.*
import doobie.implicits.*
import cats.syntax.all.* 

object SQLite:
    val xa = Transactor.fromDriverManager[IO](
        "org.sqlite.JDBC", "jdbc:sqlite:ctdose.db", None
    )

    def createTablesIfNotExists() = 
        val study = sql"""
        | CREATE TABLE IF NOT EXISTS study 
        | (acno TEXT NOT NULL, patient_id TEXT NOT NULL REFERENCES patient (id),
        |  study_datetime DATETIME NOT NULL, studyname TEXT NOT NULL, 
        |  studyprotocol TEXT, bodypart TEXT, 
        |  station TEXT, operator TEXT, 
        |  doseValue REAL NOT NULL DEFAULT 0.0, img BLOB) 
        """.stripMargin.update.run

        val patient = sql"""
        | CREATE TABLE IF NOT EXISTS patient 
        | (id TEXT NOT NULL, sex INTEGER, birthday DATE)
        """.stripMargin.update.run

        val log = sql"""
        | CREATE TABLE IF NOT EXISTS log 
        | (at DATETIME NOT NULL DEFAULT current_timestamp, 
        | logtype INTEGER NOT NULL DEFAULT 0, content TEXT NOT NULL)
        """.stripMargin.update.run

        (patient, log, study).mapN(_ + _ + _).transact(xa)
        // patient.combine(log).combine(study).transact(xa)
        // patient.combine(log).transact(xa)