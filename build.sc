import mill._
import scalalib._
import scalafmt._
import publish._

import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion

object ivys {
  val chisel3 = ivy"edu.berkeley.cs::chisel3:3.5.3"
  val chisel3Plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.3"
  val sourcecode = ivy"com.lihaoyi::sourcecode:0.2.7"
  val utest = ivy"com.lihaoyi::utest:0.7.10"
  // TODO
  // val diplomacy = ivy"org.chipsalliance::diplomacy:0.0.0-327-252005"
}

// TODO: add 2.13 after chisel publish to 2.13
object tilelink extends mill.Cross[tilelink]("2.12.13")

class tilelink(val crossScalaVersion: String) extends GeneralChiselModule {
  override def ivyDeps = super.ivyDeps() ++ Agg(ivys.sourcecode)
}

/*
object diplomaticTilelink extends mill.Cross[diplomaticTilelink]("2.12.13")

class diplomaticTilelink(val crossScalaVersion: String) extends GeneralChiselModule {
  def diplomacyModule: Option[PublishModule] = None

  override def ivyDeps = super.ivyDeps() ++ (if (diplomacyModule.isEmpty) Some(ivys.diplomacy) else None)
}
*/

trait GeneralChiselModule extends CrossScalaModule with ScalafmtModule with PublishModule {
  def chisel3Module: Option[PublishModule] = None

  override def moduleDeps = Seq() ++ chisel3Module

  override def scalacPluginIvyDeps = if (chisel3Module.isEmpty) Agg(ivys.chisel3Plugin) else Agg.empty[Dep]

  override def ivyDeps = Agg() ++
    (if (chisel3Module.isEmpty) Some(ivys.chisel3) else None)

  object tests extends Tests with TestModule.Utest {
    override def ivyDeps = Agg(ivys.utest)
  }

  def publishVersion = de.tobiasroeser.mill.vcs.version.VcsVersion.vcsState().format()

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "org.chipsalliance",
    url = "https://www.github.com/sequencer/tilelink",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("sequencer", "tilelink"),
    developers = Seq(
      Developer("sequencer", "Jiuyang Liu", "https://github.com/sequencer")
    )
  )

  override def sonatypeUri:         String = "https://s01.oss.sonatype.org/service/local"
  override def sonatypeSnapshotUri: String = "https://s01.oss.sonatype.org/content/repositories/snapshots"
  def githubPublish = T {
    os.proc("gpg", "--import", "--no-tty", "--batch", "--yes")
      .call(stdin = java.util.Base64.getDecoder.decode(sys.env("PGP_SECRET").replace("\n", "")))
    val PublishModule.PublishData(artifactInfo, artifacts) = publishArtifacts()
    new SonatypePublisher(
      sonatypeUri,
      sonatypeSnapshotUri,
      s"${sys.env("SONATYPE_USERNAME")}:${sys.env("SONATYPE_PASSWORD")}",
      true,
      Seq(
        s"--passphrase=${sys.env("PGP_PASSPHRASE")}",
        "--no-tty",
        "--pinentry-mode=loopback",
        "--batch",
        "--yes",
        "-a",
        "-b"
      ).flatMap(_.split("[,]")),
      60000,
      5000,
      T.log,
      120000,
      true
    ).publish(artifacts.map { case (a, b) => (a.path, b) }, artifactInfo, true)
  }
}
