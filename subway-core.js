export default `
:- dynamic(selected_meals/1).
:- dynamic(selected_meats/1).
:- dynamic(selected_sides/1).
:- dynamic(selected_topups/1).
:- dynamic(selected_veggies/1).
:- dynamic(selected_breads/1).
:- dynamic(selected_sauces/1).
:- dynamic(options/1).
:- dynamic(createDOMV2/1).
:- dynamic(createDOMV1/1).
:- dynamic(member/2).
:- dynamic(displayOrder/1).

% Declare facts for the different types of ingredients
meals([normal, healthy, veggie, vegan, value]).
breads([wheat, honey_oat, italian, hearty_italian, flatbread]).
meats([chicken, beef, ham, bacon, salmon, tuna, turkey]).
veggies([cucumber, green_peppers, lettuce, red_onions, tomatoes]).
fatty_sauces([chipotle, bbq, ranch, sweet_chilli, mayo]).
non_fatty_sauces([honey_mustard, sweet_onion]).
cheese_topups([american, monterey_jack, cheddar]).
non_cheese_topups([avocado, egg_mayo]).
sides([chips, cookies, hashbrowns, drinks]).

% Check for meal types
is_healthy_meal(healthy).
is_value_meal(value).
is_vegan_meal(vegan).
is_veggie_meal(veggie).

% Get the valid ingredients based on current arguments 
% e.g get_meats(X) will return empty if the selected meal type is veggie

get_meals(X) :- meals(X).

get_breads(X) :- breads(X).

% No meat for vegan/veggie meal types
get_meats(X) :- findall(X, (selected_meals(Y), \\+is_vegan_meal(Y), \\+is_veggie_meal(Y), meats(X)), X).

get_veggies(X) :- veggies(X).

% No sauces for healthy meal type
get_sauces(X) :- findall(X, (selected_meals(Y), is_healthy_meal(Y) -> non_fatty_sauces(X);
                 fatty_sauces(L1), non_fatty_sauces(L2), append(L1, L2, X)), X).

% No topup for value meal type
% No cheese topup for vegan meal type
get_topups(X) :- findall(X, (selected_meals(Y), \\+is_value_meal(Y) -> (is_vegan_meal(Y) -> non_cheese_topups(X);
                 cheese_topups(L1), non_cheese_topups(L2), append(L1, L2, X))), X).

get_sides(X) :- sides(X).

% options is used get the list based on current arguments and creates the relevant HTML DOMs for GUI
options(meals) :- get_meals(L), createDOMV1(L).
options(sauces) :- get_sauces(L), createDOMV2(L).
options(breads) :- get_breads(L), createDOMV1(L).
options(meats) :- get_meats(L), createDOMV2(L).
options(veggies) :- get_veggies(L), createDOMV1(L).
options(topups) :- get_topups(L), createDOMV2(L).
options(sides) :- get_sides(L), createDOMV1(L).

% selected is used to assert facts based on the given argument
% only will assert if X is not in the selected list
selected(X,meals) :- \\+check_selection(X, meals) -> asserta(selected_meals(X)).
selected(X,breads) :- \\+check_selection(X, breads) -> asserta(selected_breads(X)).
selected(X,meats) :- \\+check_selection(X, meats) ->asserta(selected_meats(X)).
selected(X,veggies) :- \\+check_selection(X, veggies) ->asserta(selected_veggies(X)).
selected(X,sauces) :- \\+check_selection(X, sauces) ->asserta(selected_sauces(X)).
selected(X,topups) :- \\+check_selection(X, topups) ->asserta(selected_topups(X)).
selected(X,sides) :- \\+check_selection(X, sides) ->asserta(selected_sides(X)).

% Check if X is already in selected list
check_selection(X, breads):- 
selected_breads(L), member(X,L),!. 

check_selection(X, meats):- 
selected_meats(L), member(X,L),!.

check_selection(X, meals):- 
selected_meals(L), member(X,L),!.

check_selection(X, veggies):- 
selected_veggies(L), member(X,L),!.

check_selection(X, sauces):-
selected_sauces(L), member(X,L),!.

check_selection(X, topups):- 
selected_topups(L), member(X,L),!.

check_selection(X, sides):- 
selected_sides(L), member(X,L),!.

% Get user corresponding choice
% findall(X, pred(X), List) - Find possible values for predicate and display them on the GUI

show_meals(Meals) :- 
findall(X, selected_meals(X), Meals), displayOrder(Meals).

show_breads(Breads) :-
findall(X, selected_breads(X), Breads), displayOrder(Breads).

show_meats(Meats) :- 
findall(X, selected_meats(X), Meats), displayOrder(Meats).

show_veggies(Veggies) :- 
findall(X, selected_veggies(X), Veggies), displayOrder(Veggies).

show_sauces(Sauces) :- 
findall(X, selected_sauces(X), Sauces), displayOrder(Sauces).

show_topups(Topups) :- 
findall(X, selected_topups(X), Topups), displayOrder(Topups).

show_sides(Sides) :- 
findall(X, selected_sides(X), Sides), displayOrder(Sides).

% If X=1, display all the selected ingredient for the final order on the GUI
displaySelections(X) :- 
(X==1) ->
write('Prolog - Displaying selections:'),
show_meals(Meals),
show_breads(Breads),
show_meats(Meats),
show_veggies(Veggies),
show_sauces(Sauces), 
show_topups(Topups),
show_sides(Sides).

%% GUI functions

% For displaying the final order
% Used to add the selected ingredients to a <a></a> and appends to the GUI

displayOrder([]). % empty list

displayOrder([H]) :- % last item in list
write(H),
create(a, A),  
atom_concat(H, '.', Y),                                       
    html(A, Y),
create(br, BR),                                     
    get_by_id('subway-contents', Parent),
    append_child(Parent, A),
    append_child(Parent, BR).
    
displayOrder([H|T]) :-  % List with items more than one
write(H),
create(a, A),  
atom_concat(H, ', ', Y),                                         
    html(A, Y),                                   
    get_by_id('subway-contents', Parent),
    append_child(Parent, A),
displayOrder(T), !. % remove item in list and call the function again

% Create menu items for each ingredient category in the GUI

createMenuItems(H) :-                                    
create(a, A),                                         
    html(A, H),
create(br, BR),                                     
    get_by_id('subway-contents', Parent),
    append_child(Parent, BR),
    append_child(Parent, A).

% Create a button for each ingredient in each category in the GUI

createButton(H) :-    
create(button, BUTTON),           
add_class(BUTTON, 'btn btn-outline-success btn-sm'), 
    set_attr(BUTTON,type, button),
    set_attr(BUTTON,value, H),
    html(BUTTON, H),
    get_by_id('btn-group', Parent),
    append_child(Parent, BUTTON).

% CreateDOMV1 is used to create the HTML DOM for the front end based on current list
% Uses createButton(H) and createMenuItems(H)

createDOMV1([]). % empty list

createDOMV1([H]) :- % last item in list
createButton(H),
createMenuItems(H).
    
createDOMV1([H|T]) :-  % List with items more than one
createButton(H),
createMenuItems(H),
createDOMV1(T), !. % remove item in list and call the function again

% createDOMV2 is the same as createDOMV1 except it's for nested lists

createDOMV2([[]]). % empty list

createDOMV2([[H]]) :- % last item in list
createButton(H),
createMenuItems(H).

% List contains more than 1 item
createDOMV2([[H|T]]) :-  
createButton(H),
createMenuItems(H),
createDOMV2([T]), !. % remove item in list and call the function again
`
