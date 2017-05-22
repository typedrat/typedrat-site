function setCommand(new_cmd) {
    let cmd = document.getElementById("header-command");
    var to_delete = cmd.textContent.length;
    var to_append = new_cmd.length;

    function append() {
        cmd.textContent += s[0];
        new_cmd = s.slice(1);
        to_append--;

        if (to_append > 0) {
            setTimeout(append, 150);
        }
    };

    function delete_() {
        cmd.textContent = cmd.textContent.slice(0, -1);
        to_delete--;

        if (to_delete > 0) {
            setTimeout(delete_, 150);
        } else {

            setTimeout(append, 150);
        }
    };

    setTimeout(delete_, 150);
}

document.addEventListener("DOMContentLoaded", function () {
    let login = document.getElementById("header-login");

    document.getElementById("header-toggle").addEventListener("click", function () {
        if (this.classList.contains("active"))
        {
            this.classList.remove("active");
            login.classList.remove("active");
        }
        else
        {
            this.classList.add("active");
            login.classList.add("active");
        }
    });
});

document.addEventListener("DOMContentLoaded", function () {
    let previews = document.getElementsByClassName("preview-area");
    for (let i = 0; i < previews.length; i++)
    {
        let preview = previews[i];

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
                        MathJax.Hub.Queue(["Typeset", MathJax.Hub, preview.lastChild]);

                        let codeblocks = preview.lastChild.getElementsByTagName("pre");
                        for (let i = 0; i < codeblocks.length; i++)
                        {
                            hljs.highlightBlock(codeblocks[i]);
                        }
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

//# sourceMappingURL=data:application/json;charset=utf8;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImhlYWRlci5qcyIsInByZXZpZXcuanMiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQzlDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBIiwiZmlsZSI6ImFsbC5qcyIsInNvdXJjZXNDb250ZW50IjpbImZ1bmN0aW9uIHNldENvbW1hbmQobmV3X2NtZCkge1xuICAgIGxldCBjbWQgPSBkb2N1bWVudC5nZXRFbGVtZW50QnlJZChcImhlYWRlci1jb21tYW5kXCIpO1xuICAgIHZhciB0b19kZWxldGUgPSBjbWQudGV4dENvbnRlbnQubGVuZ3RoO1xuICAgIHZhciB0b19hcHBlbmQgPSBuZXdfY21kLmxlbmd0aDtcblxuICAgIGZ1bmN0aW9uIGFwcGVuZCgpIHtcbiAgICAgICAgY21kLnRleHRDb250ZW50ICs9IHNbMF07XG4gICAgICAgIG5ld19jbWQgPSBzLnNsaWNlKDEpO1xuICAgICAgICB0b19hcHBlbmQtLTtcblxuICAgICAgICBpZiAodG9fYXBwZW5kID4gMCkge1xuICAgICAgICAgICAgc2V0VGltZW91dChhcHBlbmQsIDE1MCk7XG4gICAgICAgIH1cbiAgICB9O1xuXG4gICAgZnVuY3Rpb24gZGVsZXRlXygpIHtcbiAgICAgICAgY21kLnRleHRDb250ZW50ID0gY21kLnRleHRDb250ZW50LnNsaWNlKDAsIC0xKTtcbiAgICAgICAgdG9fZGVsZXRlLS07XG5cbiAgICAgICAgaWYgKHRvX2RlbGV0ZSA+IDApIHtcbiAgICAgICAgICAgIHNldFRpbWVvdXQoZGVsZXRlXywgMTUwKTtcbiAgICAgICAgfSBlbHNlIHtcblxuICAgICAgICAgICAgc2V0VGltZW91dChhcHBlbmQsIDE1MCk7XG4gICAgICAgIH1cbiAgICB9O1xuXG4gICAgc2V0VGltZW91dChkZWxldGVfLCAxNTApO1xufVxuXG5kb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKFwiRE9NQ29udGVudExvYWRlZFwiLCBmdW5jdGlvbiAoKSB7XG4gICAgbGV0IGxvZ2luID0gZG9jdW1lbnQuZ2V0RWxlbWVudEJ5SWQoXCJoZWFkZXItbG9naW5cIik7XG5cbiAgICBkb2N1bWVudC5nZXRFbGVtZW50QnlJZChcImhlYWRlci10b2dnbGVcIikuYWRkRXZlbnRMaXN0ZW5lcihcImNsaWNrXCIsIGZ1bmN0aW9uICgpIHtcbiAgICAgICAgaWYgKHRoaXMuY2xhc3NMaXN0LmNvbnRhaW5zKFwiYWN0aXZlXCIpKVxuICAgICAgICB7XG4gICAgICAgICAgICB0aGlzLmNsYXNzTGlzdC5yZW1vdmUoXCJhY3RpdmVcIik7XG4gICAgICAgICAgICBsb2dpbi5jbGFzc0xpc3QucmVtb3ZlKFwiYWN0aXZlXCIpO1xuICAgICAgICB9XG4gICAgICAgIGVsc2VcbiAgICAgICAge1xuICAgICAgICAgICAgdGhpcy5jbGFzc0xpc3QuYWRkKFwiYWN0aXZlXCIpO1xuICAgICAgICAgICAgbG9naW4uY2xhc3NMaXN0LmFkZChcImFjdGl2ZVwiKTtcbiAgICAgICAgfVxuICAgIH0pO1xufSk7XG4iLCJkb2N1bWVudC5hZGRFdmVudExpc3RlbmVyKFwiRE9NQ29udGVudExvYWRlZFwiLCBmdW5jdGlvbiAoKSB7XG4gICAgbGV0IHByZXZpZXdzID0gZG9jdW1lbnQuZ2V0RWxlbWVudHNCeUNsYXNzTmFtZShcInByZXZpZXctYXJlYVwiKTtcbiAgICBmb3IgKGxldCBpID0gMDsgaSA8IHByZXZpZXdzLmxlbmd0aDsgaSsrKVxuICAgIHtcbiAgICAgICAgbGV0IHByZXZpZXcgPSBwcmV2aWV3c1tpXTtcblxuICAgICAgICBsZXQgc291cmNlID0gcHJldmlldy5wYXJlbnROb2RlLmdldEVsZW1lbnRzQnlUYWdOYW1lKFwidGV4dGFyZWFcIilbMF07XG4gICAgICAgIHNvdXJjZS5hZGRFdmVudExpc3RlbmVyKFwia2V5dXBcIiwgXy5kZWJvdW5jZShmdW5jdGlvbiAoKSB7XG4gICAgICAgICAgICB2YXIgeGhyID0gbmV3IFhNTEh0dHBSZXF1ZXN0O1xuICAgICAgICAgICAgeGhyLnJlc3BvbnNlVHlwZSA9IFwiZG9jdW1lbnRcIjtcbiAgICAgICAgICAgIHhoci5vcGVuKFwiUE9TVFwiLCBcIi9wcmV2aWV3X21hcmtkb3duXCIsIHRydWUpO1xuXG4gICAgICAgICAgICB4aHIuYWRkRXZlbnRMaXN0ZW5lcihcInJlYWR5c3RhdGVjaGFuZ2VcIiwgZnVuY3Rpb24gKGUpIHtcbiAgICAgICAgICAgICAgICBpZiAoeGhyLnJlYWR5U3RhdGUgPT0gNClcbiAgICAgICAgICAgICAgICB7XG4gICAgICAgICAgICAgICAgICAgIGlmICh4aHIuc3RhdHVzID09IDIwMClcbiAgICAgICAgICAgICAgICAgICAge1xuICAgICAgICAgICAgICAgICAgICAgICAgcHJldmlldy5sYXN0Q2hpbGQuaW5uZXJIVE1MID0geGhyLnJlc3BvbnNlWE1MLmZpcnN0RWxlbWVudENoaWxkLmxhc3RDaGlsZC5pbm5lckhUTUw7XG4gICAgICAgICAgICAgICAgICAgICAgICBNYXRoSmF4Lkh1Yi5RdWV1ZShbXCJUeXBlc2V0XCIsIE1hdGhKYXguSHViLCBwcmV2aWV3Lmxhc3RDaGlsZF0pO1xuXG4gICAgICAgICAgICAgICAgICAgICAgICBsZXQgY29kZWJsb2NrcyA9IHByZXZpZXcubGFzdENoaWxkLmdldEVsZW1lbnRzQnlUYWdOYW1lKFwicHJlXCIpO1xuICAgICAgICAgICAgICAgICAgICAgICAgZm9yIChsZXQgaSA9IDA7IGkgPCBjb2RlYmxvY2tzLmxlbmd0aDsgaSsrKVxuICAgICAgICAgICAgICAgICAgICAgICAge1xuICAgICAgICAgICAgICAgICAgICAgICAgICAgIGhsanMuaGlnaGxpZ2h0QmxvY2soY29kZWJsb2Nrc1tpXSk7XG4gICAgICAgICAgICAgICAgICAgICAgICB9XG4gICAgICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgICAgICAgICAgZWxzZVxuICAgICAgICAgICAgICAgICAgICB7XG4gICAgICAgICAgICAgICAgICAgICAgICBjb25zb2xlLmVycm9yKHhoci5zdGF0dXNUZXh0KTtcbiAgICAgICAgICAgICAgICAgICAgfVxuICAgICAgICAgICAgICAgIH1cbiAgICAgICAgICAgIH0pO1xuXG4gICAgICAgICAgICB4aHIuc2VuZChzb3VyY2UudmFsdWUpO1xuICAgICAgICB9LCAyMDApKTtcbiAgICB9XG59KTtcbiJdfQ==
