package org.danielnixon.warthelpers

import java.io.File

import coursier._

import scalaz.\/
import scalaz.concurrent.Task

object CoursierResolver {
  def resolve(org: String, name: String, version: String): Seq[File] = {

    val start = Resolution(Set(Dependency(Module(org, name), version)))

    val repositories = Seq(
      Cache.ivy2Local,
      MavenRepository("https://repo1.maven.org/maven2")
    )

    val fetch = Fetch.from(repositories, Cache.fetch())

    val resolution = start.process.run(fetch).unsafePerformSync

    val localArtifacts: Seq[FileError \/ File] = Task.gatherUnordered(
      resolution.artifacts.map(Cache.file(_).run)
    ).unsafePerformSync

    localArtifacts.collect {
      case scalaz.\/-(f) => f
    }
  }
}
