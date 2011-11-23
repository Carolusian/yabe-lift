 var toggle = function(tagEl) {
    var input = document.getElementById('h'+tagEl.id);
    if(tagEl.className.indexOf('selected') > -1) {
        tagEl.className = 'tag';
        input.value = '';
    } else {
        tagEl.className = 'tag selected';
        input.value = $(tagEl).html();
    }
}

function prepareTags() {
    var tags = "";
    $('[name="tags_name"]').each(function(){
        tags += $(this).val() + " ";
    });
    $("#tags_name_list").val(tags);

    return true;
}