$(document).ready(function(){
    $('#retrieving').hide();
    $('#retrieved').hide();
    $('#importsubmit').on('click',function(){
      $('#retrieving').show();
      $('#retrieved').hide();
      $.get('/ajax/recipe-import', 
        {
          importsource: $('#importsource').val(),
          importurl: $('#importurl').val()
        },
        function(data, status){
          $('#title').val(data.title);
          $('#tags').val(data.tags.join(" "));
          $('#ingredients').val(data.ingredients);
          $('#servings').val(data.servings);
          $('#cooking-time-hours').val( (data.cookingTime - (data.cookingTime % 60)) / 60 );
          $('#cooking-time-minutes').val( data.cookingTime - (data.cookingTime - (data.cookingTime % 60)) );
          $('#waiting-time-hours').val( (data.waitingTime - (data.waitingTime % 60)) / 60 );
          $('#waiting-time-minutes').val( data.waitingTime - (data.waitingTime - (data.waitingTime % 60)) );
          $('#directions').val(data.directions);
          $('#retrieving').hide();
          $('#retrieved').show();
        }
    )});
  });
  