let gulp = require('gulp');
let sourcemaps = require('gulp-sourcemaps');
let postcss = require('gulp-postcss');
let cleanCSS = require('gulp-clean-css');
let concat = require('gulp-concat');


gulp.task('css', function () {
    return gulp.src('./static/src/css/*.css')
        .pipe(sourcemaps.init())
        .pipe(postcss())
        .pipe(cleanCSS())
        .pipe(concat('all.css'))
        .pipe(sourcemaps.write())
        .pipe(gulp.dest('./static/out/css/'));
});
