let gulp = require('gulp'),
    watch = require('gulp-watch'),
    sourcemaps = require('gulp-sourcemaps'),
    postcss = require('gulp-postcss'),
    cleanCSS = require('gulp-clean-css'),
    concat = require('gulp-concat');

gulp.task('css', function () {
    return gulp.src('./static/src/css/*.css')
        .pipe(sourcemaps.init())
        .pipe(postcss())
        .pipe(cleanCSS())
        .pipe(concat('all.css'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest('./static/out/css/'));
});

gulp.task('watch', ['css'], function () {
    gulp.watch('./static/src/css/*.css', ['css']);
});
