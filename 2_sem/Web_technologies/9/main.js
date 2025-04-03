$(document).ready(function() {
    function loadTasks() {
        const tasks = JSON.parse(localStorage.getItem('tasks')) || [];
        //tasks.clear();
        tasks.sort((a, b) => a.completed - b.completed || a.text.localeCompare(b.text));
        tasks.forEach(task => {
            addTaskToList(task.text, task.completed);
        });
    }

    function addTaskToList(taskText, completed = false) {
        const listItem = $('<li></li>');
        const taskSpan = $('<span></span>').text(taskText);
        
        if (completed) {
            taskSpan.addClass('completed');
        }
        
        const deleteButton = $('<button>Удалить</button>');
        deleteButton.click(function() {
            $(this).parent().remove();
            saveTasks(); 
        });
        
        listItem.append(taskSpan).append(deleteButton);

        taskSpan.click(function() {
            $(this).toggleClass('completed');
            saveTasks(); 
        });
        
        $('#todoList').append(listItem);
        
    }

    function sortTasks() {
        const tasks = JSON.parse(localStorage.getItem('tasks')) || [];
        tasks.sort((a, b) => a.completed - b.completed || a.text.localeCompare(b.text));
        $('#todoList').empty();
        tasks.forEach(task => {
            addTaskToList(task.text, task.completed);
        });
    }

    $('#sortTasks').click(function() {
        sortTasks();
    });

    function saveTasks() {
        const tasks = [];
        $('#todoList li').each(function() {
            const taskText = $(this).find('span').text().trim();
            const completed = $(this).find('span').hasClass('completed');
            tasks.push({ text: taskText, completed });
        });
        localStorage.setItem('tasks', JSON.stringify(tasks));
    }

    loadTasks();

    $('#addTask').click(function() {
        const taskText = $('#taskInput').val().trim();
        if (taskText) {
            addTaskToList(taskText);
            $('#taskInput').val('');
            saveTasks(); 
            
        }
    });

    $('#taskInput').keypress(function(e) {
        if (e.which === 13) {
            $('#addTask').click();
            
        }
    });
});