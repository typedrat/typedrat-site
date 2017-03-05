module.exports = {
    plugins: [
        require('postcss-cssnext')(),
        require('postcss-google-color')(),
        require('lost')()
    ]
}
