let gulp = require('gulp');
let postcss = require('gulp-postcss');
let rename = require('gulp-rename');

gulp.task('css', function () {
    return gulp.src('./static/src/css/*.pcss')
        .pipe(postcss())
        .pipe(rename({extname: ".css"}))
        .pipe(gulp.dest('./static/out/css/'));
});
