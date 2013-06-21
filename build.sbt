import AssemblyKeys._ // put this at the top of the file

assemblySettings

name := "Shalene GUV Analyzer"

version := "1.0a"

scalaVersion := "2.10.2"

javaHome := Some(new File("/Library/Java/JavaVirtualMachines/jdk1.7.0_25.jdk/Contents/Home"))

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")

libraryDependencies += "org.jfxtras" % "jfxtras-labs" % "2.2-r5"

mainClass in (Compile, run) := Some("shalene.Launch")

unmanagedJars in Compile <+= javaHome map { jh => Attributed.blank( new File( jh.getOrElse(sys.error("Error, could not get java home")),"jre/lib/jfxrt.jar" ) ) }

retrieveManaged := true

jarName in assembly := "ShaleneGUV.jar"

mainClass in assembly := Some("shalene.Launch")

