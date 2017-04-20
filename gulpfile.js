var gulp = require('gulp');
var elm = require('gulp-elm');
var gutil = require('gulp-util');
var plumber = require('gulp-plumber');
var connect = require('gulp-connect');

//file paths
var paths = {
    dest: 'dist',
    elm: 'src/*.elm',
    static: 'src/*.{html,css}'
};

//init elm
gulp.task('elm-init', elm.init);

//compile elm to html
gulp.task('elm', ['elm-init'], function() {
    return gulp.src(paths.elm)
        .pipe(plumber())
        .pipe(elm())
        .pipe(gulp.dest(paths.dest));
});

//move static assets to dist
gulp.task('static', function() {
    return gulp.src(paths.static)
        .pipe(plumber())
        .pipe(gulp.dest(paths.dest));
});

//watch for changes
gulp.task('watch', function() {
    gulp.watch(paths.elm, ['elm']);
    gulp.watch(paths.static, ['static']);
});

//create server
gulp.task('connect', function() {
    connect.server({
        root: 'dist',
        port: 5050
    });
});


//main tasks
gulp.task('build', ['elm', 'static']);
gulp.task('default', ['connect', 'build', 'watch']);