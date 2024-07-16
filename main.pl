:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- dynamic(task/2).

% Routes
:- http_handler(root(.), main_page, []).
:- http_handler(root(tasks), add_task_handler, [method(post)]).

% Static files
http:location(static, '/static', []).
:- http_handler(static(.), http_reply_from_files(static, []), [prefix]).

app_name('Prolog TODO 📝').

main_page(_Request) :-
    app_name(Title),
    findall(Task, task(Task, _), Tasks),
    reply_html_page(
        % HTML head
        [title(Title)],
        % HTML body
        [
            \html_requires('/static/style.css'),
            \page_content(Title, Tasks)
        ]
    ).
    
page_content(Title, Tasks) -->
    html(
        [
            h2(Title),
            form([ action='/tasks', method='post' ],
                [ label([ for='task' ], 'Task: '),
                    input([ type='text', id='task-input', name='task', required ]),
                    input([ type='submit', value='Add' ])
                ]),
            h3('Tasks'),
            div([ id='task-list' ],
                [ ul(\print_tasks(Tasks)) ]
            )
        ]
    ).

% DCG to print tasks in HTML
print_tasks([]) --> [].
print_tasks([Task|Rest]) -->
    html(li(Task)),
    print_tasks(Rest).

% Handler for creating tasks
add_task_handler(Request) :-
    http_parameters(Request, [task(Task, [atom])]),
    assertz(task(Task, _)),
    main_page(Request),
    http_redirect(see_other, root(.), _).

% Start server on port 8000
server(Port) :-
    http_server(http_dispatch, [port(Port)]).
:- initialization(server(8000)).

% TODO: add task IDs
% TODO: edit and delete tasks
% TODO: csv storage
