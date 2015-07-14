/* jshint node: true */
"use strict";

var gulp = require("gulp"),
    purescript = require("gulp-purescript"),
    run = require("gulp-run"),

    sources = [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
      "test/**/*.purs"
    ],

    foreigns = [
      "src/**/*.js",
      "bower_components/purescript-*/src/**/*.js",
      "test/**/*.js"
    ];

gulp.task("make", function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("test", ["make"], function() {
  return purescript.pscBundle({ src: "output/**/*.js", main: "Test.Main" })
    .pipe(run("node"));
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task("default", ["test", "dotpsci"]);
