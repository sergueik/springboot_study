$(function() {

    const appendTodo = function(data) {
        var todoCode =
            '<a href="#" class="todo-link" data-id="' + data.id + '">' + data.name + '</a>' +
            '<br>';
        $('#todo-list')
            .append('<div class="todo-div" id="' + data.id + '">' + todoCode + '</div>');
    };

    //Adding _todo
    const addingTodo = function() {
        $('#save-todo').click(function() {
            var data = $('#todo-form form').serialize();
            $.ajax({
                method: "POST",
                url: '/todo-list/',
                data: data,
                success: function(response) {
                    $('#todo-form').css('display', 'none');
                    var todo = {};
                    todo.id = response;
                    var dataArray = $('#todo-form form').serializeArray();
                    for (i in dataArray) {
                        todo[dataArray[i]['name']] = dataArray[i]['value'];
                    }
                    appendTodo(todo);
                }
            });
            return false;
        });
    };

    const codeDataTodo = function(response, todoId) {
        return '<span id="' + todoId + '">' +
            'Описание: ' + response.description + '<br>' +
            'Дата: ' + response.date +
            '<br>' +
            '<button id="delete-todo" data-id="' + response.id + '">Удалить дело</button>' +
            '<button id="show-update-todo-list" ' +
            'data-id="' + response.id + '" ' +
            'data-name="' + response.name + '" ' +
            'data-description="' + response.description + '" ' +
            'data-date="' + response.date + '">Изменить дело</button>' +
            '</span>';
    };

    const todoInputValue = function(name, description, date) {
        $('#todo-form-name').val(name);
        $('#todo-form-description').val(description);
        $('#todo-form-date').val(date);
    };

    const todoFormNameAndButton = function(formName, formButton, idButton) {
        $('#todo-form > form > h2').remove();
        $('#todo-form > form > button').remove();
        $('#todo-form > form')
            .prepend('<h2>' + formName + '</h2>')
            .append('<button id="' + idButton + '">' + formButton + '</button>');
    };

    //Show adding _todo form and adding _todo
    $('#show-add-todo-list').click(function() {
        todoFormNameAndButton('Добавить дело', 'Добавить', 'save-todo');
        todoInputValue('', '', '');
        $('#todo-form').css('display', 'flex');
        addingTodo();
    });

    //Closing _todo form
    $('#todo-form').click(function(event) {
        if (event.target === this) {
            $(this).css('display', 'none');
        }
    });

    //Getting _todo
    $(document).on('click', '.todo-link', function() {
        var link = $(this);
        var todoId = link.data('id');
        $.ajax({
            method: "GET",
            url: '/todo-list/' + todoId,
            success: function(response) {
                if ($('.todo-div > span').is('#' + todoId)) {
                    return;
                }
                link.parent().append(codeDataTodo(response, todoId));
            },
            error: function(response) {
                if (response.status == 404) {
                    alert('Дело не найдено!');
                }
            }
        });
        return false;
    });

    //Update _todo and show updating _todo form
    $(document).on('click', '#show-update-todo-list', function() {
        var buttonUpdate = $(this);
        var todoId = buttonUpdate.data('id');
        var todoName = buttonUpdate.data('name');
        var todoDescription = buttonUpdate.data('description');
        var todoDate = buttonUpdate.data('date');
        todoFormNameAndButton('Изменить дело', 'Изменить', 'update-todo');
        todoInputValue(todoName, todoDescription, todoDate);
        $('#todo-form').css('display', 'flex');
        $('#update-todo').click(function() {
            var data = $('#todo-form form').serialize();
            $.ajax({
                method: "PUT",
                url: '/todo-list/' + todoId,
                data: data,
                success: function(response) {
                    $('#todo-form').css('display', 'none');
                    response.date = response.date.slice(0, 10);
                    $('.todo-div#' + todoId + ' > a').text(response.name);
                    $('.todo-div#' + todoId + ' > span').replaceWith(codeDataTodo(response, todoId));
                }
            });
            return false;
        });
    });

    //Delete _todo
    $(document).on('click', '#delete-todo', function() {
        var buttonDelete = $(this);
        var todoId = buttonDelete.data('id');
        $.ajax({
            method: "DELETE",
            url: '/todo-list/' + todoId,
            success: function() {
                $('.todo-div#' + todoId).remove();
            }
        });
        return false;
    });
});