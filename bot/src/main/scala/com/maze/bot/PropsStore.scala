package com.maze.bot

import java.io.FileOutputStream
import java.util.Properties

import scala.io.Source

object PropsStore {

  import resource._

  private val fileName = "props.store"

  val properties = new Properties()

  for (reader <- managed(Source.fromFile(fileName).reader())) properties.load(reader)

  def save(key: String, value: String): Unit = {
    properties.put(key, value)
    for (out <- managed(new FileOutputStream(fileName))) properties.store(out, null)
  }
}
