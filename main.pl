:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).

:- dynamic(task/2).

% Define HTTP handlers
:- http_handler(root(.), home_page, []).
:- http_handler(root(tasks), tasks_handler, [method(post)]).

% Home page handler
home_page(_Request) :-
    format('Content-type: text/html~n~n'),
    format('<html>
                <head><title>ToDo List</title></head>
                <body>
                    <h2>ToDo List</h2>
                    <form action="/tasks" method="post">
                        <label for="task">Task:</label>
                        <input type="text" id="task" name="task" required>
                        <input type="submit" value="Add Task">
                    </form>
                    <h3>Tasks</h3>
                    <ul>~n'),
    findall(Task, task(Task, _), Tasks),
    print_tasks(Tasks),
    format('</ul>
            </body>
            </html>').

% Predicate to print tasks in HTML
print_tasks([]).
print_tasks([Task|Rest]) :-
    format('<li>~w</li>~n', [Task]),
    print_tasks(Rest).

% Handler for creating tasks
tasks_handler(Request) :-
    http_parameters(Request, [task(Task, [atom])]),
    assertz(task(Task, _)),
    reply_json_dict(_{status:success}).

% Start server
server(Port) :-
    http_server(http_dispatch, [port(Port)]).

% Example usage: Start server on port 8080
:- initialization(server(8080)).
