document.addEventListener("DOMContentLoaded", function () {
    let previews = document.getElementsByClassName("preview-area");
    for (let i = 0; i < previews.length; i++)
    {
        let preview = previews.item(i);

        let source = preview.parentNode.getElementsByTagName("textarea")[0];
        source.addEventListener("keyup", _.debounce(function () {
            var xhr = new XMLHttpRequest;
            xhr.responseType = "document";
            xhr.open("POST", "/preview_markdown", true);

            xhr.addEventListener("readystatechange", function (e) {
                if (xhr.readyState == 4)
                {
                    if (xhr.status == 200)
                    {
                        preview.lastChild.innerHTML = xhr.responseXML.firstElementChild.lastChild.innerHTML;
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, preview.lastChild.innerHTML]);
                    }
                    else
                    {
                        console.error(xhr.statusText);
                    }
                }
            });

            xhr.send(source.value);
        }, 200));
    }
});
