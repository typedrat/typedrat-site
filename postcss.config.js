module.exports = {
    plugins: [
        require('postcss-cssnext')(),
        require('lost')(),
        require('postcss-google-color')({
            defaultLevel: 500
        })
    ],
    map: true
}
