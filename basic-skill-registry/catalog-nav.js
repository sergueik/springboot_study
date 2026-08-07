(function () {

    function filterTable() {
        var input = document.getElementById("catalog-search");
        var text = input.value.toLowerCase();

        var rows = document.querySelectorAll("#catalog tbody tr");
	// searches for all columns
        rows.forEach(function(row) {
            var visible =
                row.textContent.toLowerCase().indexOf(text) >= 0;

            row.style.display = visible ? "" : "none";
        });
    }


    function enableSearch() {
        var input = document.getElementById("catalog-search");

        if (input) {
            input.addEventListener("keyup", filterTable);
        }
    }


    function enableSorting() {
        var headers =
            document.querySelectorAll("#catalog th");

        headers.forEach(function(header, index) {

            header.addEventListener("click", function() {

                var table =
                    document.getElementById("catalog");

                var body = table.tBodies[0];

                var rows =
                    Array.from(body.rows);

                rows.sort(function(a,b) {
                    return a.cells[index].textContent
                        .localeCompare(
                            b.cells[index].textContent
                        );
                });

                rows.forEach(function(row) {
                    body.appendChild(row);
                });

            });

        });
    }


    window.onload = function() {
        enableSearch();
        enableSorting();
    };

})();
