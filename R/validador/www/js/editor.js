editor = function(seletor) {
    tinymce.init({
        selector: seletor,
        height: 400,
        menubar: false,
        plugins: [
            'advlist lists link image charmap print hr pagebreak',
            'searchreplace wordcount visualblocks visualchars fullscreen',
            'table code paste textcolor colorpicker help'
        ],
        toolbar1: 'undo redo | insert | styleselect | removeformat forecolor bold italic strikethrough underline table',
        toolbar2: 'alignleft aligncenter alignright alignjustify | outdent indent  | bullist numlist | link image | print help',
        image_advtab: true,
        encoding: 'UTF-8'

        // ver estilo das tabelas
    });
};