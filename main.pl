:- use_module(library(http/thread_httpd)). % for http_server/2
:- use_module(library(http/http_dispatch)). % for http_handler/3
:- use_module(library(http/http_parameters)). % for http_parameters/2
:- use_module(library(http/http_files)). % for static files
:- use_module(library(http/html_write)). % for html//1
:- use_module(library(http/html_head)). % for html_requires/1

:- dynamic(task/2). % task(ID, Description)

% Routes
:- http_handler(root(.), main_page, []).
:- http_handler(root(add_task), add_task_handler, [method(post)]).
:- http_handler(root(delete_task), delete_task_handler, [method(post)]).

% Static files
http:location(static, '/static', []).
:- http_handler(static(.), http_reply_from_files(static, []), [prefix]).

app_name('Prolog TODO 📝').

main_page(_Request) :-
    app_name(Title),
    findall([ID, Description], task(ID, Description), Tasks),
    reply_html_page(
        % HTML head
        [title(Title), link([rel='icon', type='image/png', href='/static/favicon.png'])],
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
            form([ action='/add_task', method='post', id='add-task-form' ],
                [
                    label([ for='task' ], 'Task'),
                    input([ type='text', id='task-input', name='task', required ]),
                    input([ type='submit', id='add-btn', value='Add' ])
                ]),
            h3('Tasks'),
            div([ id='task-list' ],
                [ ul(\print_tasks(Tasks)) ]
            )
        ]
    ).

% DCG to print tasks in HTML
print_tasks([]) --> [].
print_tasks([[TaskId, Description]|Rest]) -->
    html(li([class='task'], [
        form([action='/delete_task', method='post'], [
            input([type='submit', class='delete-btn', value='x']),
            Description,
            input([type='hidden', name='task_id', value=TaskId])
        ])
    ])),
    print_tasks(Rest).

% Handlers for creating and deleting tasks
add_task_handler(Request) :-
    http_parameters(Request, [task(Description, [atom])]),
    generate_task_id(NewId), 
    assertz(task(NewId, Description)),
    http_redirect(see_other, root(.), _).

delete_task_handler(Request) :-
    http_parameters(Request, [task_id(TaskID, [integer])]),
    retract(task(TaskID, _)),
    http_redirect(see_other, root(.), _).

% Utilities
generate_task_id(NewId) :-
    (findall(Id, task(Id, _), Ids),
        % Increment the ID
        Ids \= [] ->
            max_list(Ids, MaxId),
            NewId is MaxId + 1
        ; % If no tasks exist, start with 1
            NewId = 1
    ).

% Start server on port 8000
server(Port) :-
    http_server(http_dispatch, [port(Port)]).
:- initialization(server(8000)).

% TODO: add task IDs
% TODO: edit and delete tasks
% TODO: csv storage
