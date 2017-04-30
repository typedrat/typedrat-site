let gulp = require('gulp'),
    watch = require('gulp-watch'),
    sourcemaps = require('gulp-sourcemaps'),
    postcss = require('gulp-postcss'),
    cleanCSS = require('gulp-clean-css'),
    concat = require('gulp-concat'),
    babel = require('gulp-babel');

gulp.task('css', function () {
    return gulp.src('./static/src/css/*.css')
        .pipe(sourcemaps.init())
        .pipe(postcss())
        .pipe(cleanCSS())
        .pipe(concat('all.css'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest('./static/out/'));
});

gulp.task('js', function () {
    return gulp.src('./static/src/js/*.js')
        .pipe(sourcemaps.init())
        .pipe(babel({

        }))
        .pipe(concat('all.js'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest('./static/out/'));
});

gulp.task('watch', ['css', 'js'], function () {
    gulp.watch('./static/src/css/*.css', ['css']);
    gulp.watch('./static/src/js/*.js', ['js']);
});
