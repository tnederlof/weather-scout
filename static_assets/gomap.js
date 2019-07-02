// Allow a user hitting enter to trigger a search like they clicked the button
$(document).keyup(function(event) {
    if ($("#target_zone").is(":focus") && (event.key == "Enter")) {
        $("#search_target_zone").click();
    }
});