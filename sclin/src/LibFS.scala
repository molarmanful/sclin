package sclin

import better.files.*
import java.io.File as JFile
import java.nio.file.StandardWatchEventKinds
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.nio.*
import monix.reactive.Consumer
import monix.reactive.Observable
import scala.collection.immutable.VectorMap
import scala.io.StdIn.*
import scala.util.chaining.*
import ANY.*

extension (env: ENV)

  def in: ENV   = env.push(STR(readLine))
  def out: ENV  = env.arg1((x, env) => env.tap(_ => print(x)))
  def outn: ENV = env.arg1((x, env) => env.tap(_ => println(x)))

  def form: ENV = env.mod1(_.toForm.sSTR)
  def outf: ENV = env.form.outn

  def pathARR: ENV = env.mod1:
    _.toFile.toString
      .split(JFile.separator)
      .filter(_.nonEmpty)
      .map(_.sSTR)
      .toVector
      .toARR
  def pathSTR: ENV  = env.mod1(_.toFile.toString.sSTR)
  def pathname: ENV = env.mod1(_.toFile.name.sSTR)
  def pathbase: ENV = env.mod1(_.toFile.nameWithoutExtension.sSTR)
  def pathext: ENV  = env.mod1(_.toFile.extension.getOrElse("").sSTR)

  def fsread: ENV = env.mod1: x =>
    Task(x.toFile.newInputStream)
      .pipe(Observable.fromInputStream(_))
      .pipeThrough(text.UTF8Codec.utf8Decode)
      .map(_.sSTR)
      .toOBS
  def fsreadb: ENV = env.mod1: x =>
    Task(x.toFile.newInputStream)
      .pipe(Observable.fromInputStream(_))
      .map(Util.abtobs(_).sSTR)
      .toOBS
  def fsreadn: ENV = env.mod2: (x, y) =>
    y.vec1: s =>
      Task(x.toFile.newBufferedReader(Util.charset(s.toString)))
        .pipe(Observable.fromLinesReader)
        .map(_.sSTR)
        .toOBS

  def fswH(f: ANY => Consumer[Array[Byte], Long])(x: ANY, y: ANY): ANY =
    x.toOBS.x
      .map(_.toString)
      .pipeThrough(text.UTF8Codec.utf8Encode)
      .consumeWith(f(y))
      .map(NUM(_))
      .toTASK
  def fswbH(f: ANY => Consumer[Array[Byte], Long])(x: ANY, y: ANY): ANY =
    x.toOBS.x
      .map(_.toString.pipe(Util.bstoab))
      .consumeWith(f(y))
      .map(NUM(_))
      .toTASK

  def fswrite: ENV  = env.mod2(fswH(f => file.writeAsync(f.toFile.path)))
  def fswriteb: ENV = env.mod2(fswbH(f => file.writeAsync(f.toFile.path)))
  def fswriteat: ENV = env.mod3: (x, y, z) =>
    z.vec1(n => fswH(f => file.appendAsync(f.toFile.path, n.toInt))(x, y))
  def fswriteatb: ENV = env.mod3: (x, y, z) =>
    z.vec1(n => fswbH(f => file.appendAsync(f.toFile.path, n.toInt))(x, y))

  def fswatch: ENV = env.mod1:
    _.toFile.path.pipe:
      file
        .watchAsync(_)
        .map:
          _.map: ev =>
            VectorMap(
              STR("t") -> NUM:
                ev.kind match
                  case StandardWatchEventKinds.ENTRY_CREATE => 1
                  case StandardWatchEventKinds.ENTRY_MODIFY => 2
                  case StandardWatchEventKinds.ENTRY_DELETE => 0
              ,
              STR("n") -> ev.count.pipe(NUM(_)),
              STR("f") -> ev.context.toString.sSTR
            ).toMAP
          .toARR
        .toOBS

  def fslsH[T](x: ANY)(f: File => Iterator[File]): ANY =
    Task(x.toFile.pipe(f).map(_.toString.sSTR))
      .pipe(Observable.fromIterator(_))
      .toOBS

  def fsls: ENV  = env.mod1(fslsH(_)(_.list))
  def fslsr: ENV = env.mod1(fslsH(_)(_.listRecursively))

  def fsglob: ENV =
    env.mod2((x, y) => y.vec1(s => fslsH(x)(_.glob(s.toString))))
  def fsglobR: ENV =
    env.mod2((x, y) => y.vec1(s => fslsH(x)(_.globRegex(s.toString.r))))

  def tcpread: ENV = env.vec2: (x, y) =>
    tcp
      .readAsync(x.toString, y.toInt)
      .pipeThrough(text.UTF8Codec.utf8Decode)
      .map(_.sSTR)
      .toOBS
  def tcpreadb: ENV = env.vec2: (x, y) =>
    tcp.readAsync(x.toString, y.toInt).map(Util.abtobs(_).sSTR).toOBS

  def tcpwrite: ENV = env.mod3: (x, y, z) =>
    y.vec2(z)((s, n) => fswH(_ => tcp.writeAsync(s.toString, n.toInt))(x, UN))
  def tcpwriteb: ENV = env.mod3: (x, y, z) =>
    y.vec2(z)((s, n) => fswbH(_ => tcp.writeAsync(s.toString, n.toInt))(x, UN))

  def btou: ENV = env.str1(Util.bstoab(_).pipe(String(_, "UTF-8")))
  def utob: ENV = env.str1(_.getBytes.pipe(Util.abtobs))

  def oBtoU: ENV = env.mod1: x =>
    x.toOBS.x
      .map(_.toString.pipe(Util.bstoab))
      .pipeThrough(text.UTF8Codec.utf8Decode)
      .map(_.sSTR)
      .toOBS
  def oUtoB: ENV = env.mod1: x =>
    x.toOBS.x
      .map(_.toString)
      .pipeThrough(text.UTF8Codec.utf8Encode)
      .map(Util.abtobs(_).sSTR)
      .toOBS
