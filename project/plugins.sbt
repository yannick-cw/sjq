addSbtPlugin("com.lucidchart"            % "sbt-scalafmt"        % "1.11")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"        % "0.1.5")
addSbtPlugin("com.typesafe.sbt"          % "sbt-native-packager" % "1.3.20")
addSbtPlugin("com.eed3si9n"              % "sbt-assembly"        % "0.14.9")
addSbtPlugin("com.holidaycheck.soil.sbt" % "sbt-docker"          % "7.4.0")
addSbtPlugin("com.github.gseitz"         % "sbt-release"         % "1.0.11")

resolvers += "internal.release-repo" at "http://nexus.dev.hcg.cloud/nexus/content/repositories/releases"
