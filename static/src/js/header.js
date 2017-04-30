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

    document.getElementById("header-lambda").addEventListener("click", function () {
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
