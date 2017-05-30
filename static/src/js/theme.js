document.addEventListener("DOMContentLoaded", () => {
    const d = new Date;

    if (d.getHours() < 9 || d.getHours() >= 17)
    {
        let body = document.getElementsByTagName("body")[0];
        body.classList.remove("light");
        body.classList.add("dark");
    }
});
