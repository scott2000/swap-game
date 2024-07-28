name := "swapgame"

import android.Keys._
android.Plugin.androidBuild

javacOptions ++= Seq("-source", "1.7", "-target", "1.7")
scalaVersion := "2.11.7"
scalacOptions in Compile += "-feature"

platformTarget in Android := "android-34"

updateCheck in Android := {}
proguardCache in Android ++= Seq("org.scaloid")

proguardOptions in Android ++= Seq("-dontobfuscate", "-dontoptimize", "-keepattributes Signature", "-printseeds target/seeds.txt", "-printusage target/usage.txt"
  , "-dontwarn scala.collection.**" // required from Scala 2.11.4
  , "-dontwarn org.scaloid.**" // this can be omitted if current Android Build target is android-16
  , "-dontwarn androidx.**"
)

resolvers += "Google" at "https://dl.google.com/android/maven2"

libraryDependencies += "org.scaloid" %% "scaloid" % "4.2"
libraryDependencies += "com.google.android.gms" % "play-services-games" % "8.4.0"
// Need to copy "sdk/build-tools/25.0.3/lib/dx.jar" into "sdk/build-tools/<VERSION>/lib"
// Need to run `apksigner sign --ks-key-alias <KEY> --ks <KEYSTORE> <APK>` to sign generated APKs
// Can use `adb install <APK>` after signing with key from project.properties

run <<= run in Android
install <<= install in Android
