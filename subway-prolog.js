export default `
:- dynamic(chosen_meals/1).
:- dynamic(chosen_meats/1).
:- dynamic(chosen_sides/1).
:- dynamic(chosen_topups/1).
:- dynamic(chosen_veggies/1).
:- dynamic(chosen_breads/1).
:- dynamic(chosen_sauces/1).
:- dynamic(options/1).
:- dynamic(createDOMV2/1).
:- dynamic(createDOMV1/1).
:- dynamic(member/2).
:- dynamic(displayOrder/1).

append([], Y, Y).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

% Checking conditions
healthy_meal(healthy).
value_meal(value).
vegan_meal(vegan).
veggie_meal(veggie).

meals([normal, healthy, veggie, vegan, value]).
breads([wheat, honey_oat, italian, hearty_italian, flatbread]).
meats([chicken, beef, ham, bacon, salmon, tuna, turkey]).
veggies([cucumber, green_peppers, lettuce, red_onions, tomatoes]).
fatty_sauces([chipotle, bbq, ranch, sweet_chilli, mayo]).
non_fatty_sauces([honey_mustard, sweet_onion]).
cheese_topups([american, monterey_jack, cheddar]).
non_cheese_topups([avocado, egg_mayo]).
sides([chips, cookies, hashbrowns, drinks]).

% Get possible meals
ask_meals(X) :- meals(X).

% Get possible breads
ask_breads(X) :- breads(X).

% Get possible meats
% Vegan and Veggie meals do not have meat options
ask_meats(X) :- findall(X, (chosen_meals(Y), \\+vegan_meal(Y), \\+veggie_meal(Y), meats(X)), X).

% Get possible veggies
ask_veggies(X) :- veggies(X).

% Get possible  sauces.
% Healthy meals do not have fatty sauces
ask_sauces(X) :- findall(X, (chosen_meals(Y), healthy_meal(Y) -> non_fatty_sauces(X);
                 fatty_sauces(L1), non_fatty_sauces(L2), append(L1, L2, X)), X).

% Get possible topups
% Value meal does not have topup and  Vegan meal does not have cheese topup.
ask_topups(X) :- findall(X, (chosen_meals(Y), \\+value_meal(Y) -> (vegan_meal(Y) -> non_cheese_topups(X);
                 cheese_topups(L1), non_cheese_topups(L2), append(L1, L2, X))), X).

% Get possible sides
ask_sides(X) :- sides(X).

% options is used get the list based on current arguments and creates the relevant HTML DOMs for GUI
options(meals) :- ask_meals(L), createDOMV1(L).
options(sauces) :- ask_sauces(L), createDOMV2(L).
options(breads) :- ask_breads(L), createDOMV1(L).
options(meats) :- ask_meats(L), createDOMV2(L).
options(veggies) :- ask_veggies(L), createDOMV1(L).
options(topups) :- ask_topups(L), createDOMV2(L).
options(sides) :- ask_sides(L), createDOMV1(L).

% selected is used to assert facts based on the given argument
% only will assert if X is not already in chose list
selected(X,meals) :- \\+check_selection(X, meals) -> asserta(chosen_meals(X)).
selected(X,breads) :- \\+check_selection(X, breads) -> asserta(chosen_breads(X)).
selected(X,meats) :- \\+check_selection(X, meats) ->asserta(chosen_meats(X)).
selected(X,veggies) :- \\+check_selection(X, veggies) ->asserta(chosen_veggies(X)).
selected(X,sauces) :- \\+check_selection(X, sauces) ->asserta(chosen_sauces(X)).
selected(X,topups) :- \\+check_selection(X, topups) ->asserta(chosen_topups(X)).
selected(X,sides) :- \\+check_selection(X, sides) ->asserta(chosen_sides(X)).



% Check if X is already in chosen list
check_selection(X, breads):- 
chosen_breads(L), member(X,L),!. 

check_selection(X, meats):- 
chosen_meats(L), member(X,L),!.

check_selection(X, meals):- 
chosen_meals(L), member(X,L),!.

check_selection(X, veggies):- 
chosen_veggies(L), member(X,L),!.

check_selection(X, sauces):-
chosen_sauces(L), member(X,L),!.

check_selection(X, topups):- 
chosen_topups(L), member(X,L),!.

check_selection(X, sides):- 
chosen_sides(L), member(X,L),!.

% Get user corresponding choice
% findall(X, pred(X), List) - Find possible values for predicate and add to the List

show_meals(Meals) :- 
write('Displaying selected meal'),
findall(X, chosen_meals(X), Meals), displayOrder(Meals).

show_breads(breads) :-
write('Displaying selected bread'),
findall(X, chosen_breads(X), Breads), displayOrder(Breads).

show_meats(meats) :- 
write('Displaying selected meats'),
findall(X, chosen_meats(X), Meats), displayOrder(Meats).

show_veggies(veggies) :- 
write('Displaying selected veggiess'),
findall(X, chosen_veggies(X), Veggies), displayOrder(Veggies).

show_sauces(sauces) :- 
write('Displaying selected sauces'),
findall(X, chosen_sauces(X), Sauces), displayOrder(Sauces).

show_topups(topups) :- 
write('Displaying selected topups'),
findall(X, chosen_topups(X), Topups), displayOrder(Topups).

show_sides(sides) :- 
write('Displaying selected sides'),
findall(X, chosen_sides(X), Sides), displayOrder(Sides).

displaySelections(X) :- 
(X==1) ->
write('Prolog - Displaying selections:'),
show_meals(Meals),
show_breads(breads),
show_meats(meats).

%show_meals(meals),
%show_breads(breads), 
%show_meats(meats),
%show_veggies(veggies),
%show_sauces(sauces), 
%show_topups(topups),
%show_sides(sides).

%% GUI functions

displayOrder([]):- % empty list
write('Empty List').

displayOrder([H]) :- % last item in list
write(H),
write('disPlayOrderLastItem'),
create(a, A),                                         
    html(A, H),
create(br, BR),                                     
    get_by_id('subway-contents', Parent),
    append_child(Parent, A),
    append_child(Parent, BR).
    
displayOrder([H|T]) :-  % List with items more than one
write(H),
create(a, A),                                         
    html(A, H ),                                   
    get_by_id('subway-contents', Parent),
    append_child(Parent, A),
displayOrder(T), !. % remove item in list and call the function again

% create menu item for GUI
createMenuItems(H) :-                                    
create(a, A),                                         
    html(A, H),
create(br, BR),                                     
    get_by_id('subway-contents', Parent),
    append_child(Parent, BR),
    append_child(Parent, A).

% Creating a GUI button for item
createButton(H) :-    
create(button, BUTTON),           
add_class(BUTTON, 'btn btn-outline-success btn-sm'), 
    set_attr(BUTTON,type, button),
    set_attr(BUTTON,value, H),
    html(BUTTON, H),
    get_by_id('btn-group', Parent),
    append_child(Parent, BUTTON).

% createDOMV1 is used to create the HTML DOM for the front end based on current list

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
