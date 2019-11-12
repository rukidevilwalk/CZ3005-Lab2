export default `
% Declare dynamic predicates for storing results
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
:- dynamic(displayOrder/2).

% Declare facts for the different types of ingredients
meals([normal, healthy, veggie, vegan, value]).
breads([multigrain, wheat, honey_oat, italian, parmesan_oregano, hearty_italian, flatbread]).
meats([beefsteak, chicken, ham, bacon, beef, meatballs, salmon, tuna, turkey]).
veggies([black_olives, jalapeno, pickles, cucumber, green_peppers, red_onions, tomatoes, lettuce]).
fat_sauces([chipotle, bbq, ranch, sweet_chilli, mayo]).
non_fat_sauces([honey_mustard, sweet_onion, yelow_mustard, deli_brown_mustard]).
non_vegan_topups([american, monterey_cheddar, processed_cheddar]).
vegan_topups([avocado, egg_mayo]).
sides([yogurt, chips, cookies, hashbrowns, drinks]).

% Declare fact for different types of behaviours
behaviour([tired_behaviour, energetic_behaviour, calm_behaviour]).

% Declare facts for the different types of staff's behaviour
behaviour(tired_behaviour, [closing_eyes, grouchy_look, listless_eyes]).
behaviour(energetic_behaviour, [broad_smile, beaming_voice, whistling]).
behaviour(calm_behaviour, [looking_composed, looking_attentive, light_smile]).

% Declare facts for meal types, used for checking the valid choices
is_healthy_meal(healthy).
is_value_meal(value).
is_vegan_meal(vegan).
is_veggie_meal(veggie).

% Declare predicates for getting the valid ingredients based on current arguments 
% e.g get_meats(X) will return empty if the selected meal type is veggie

get_meals(X) :- meals(X).

get_breads(X) :- breads(X).

% No meat for vegan/veggie meal types
get_meats(X) :- 
findall(X, (selected_meals(Y), \\+is_veggie_meal(Y), \\+is_vegan_meal(Y), meats(X)), X).

get_veggies(X) :- veggies(X).

% No sauces for healthy meal type
get_sauces(X) :- 
findall(X, (selected_meals(Y), is_healthy_meal(Y) -> non_fat_sauces(X);
non_fat_sauces(L1),  fat_sauces(L2), append(L1, L2, X)), X).

% No topup for value meal type; No cheese topup for vegan meal type
get_topups(X) :- 
findall(X, (selected_meals(Y), \\+is_value_meal(Y) -> (is_vegan_meal(Y) -> vegan_topups(X);
non_vegan_topups(L1), vegan_topups(L2), append(L1, L2, X))), X).

get_sides(X) :- sides(X).

% Declare predicates for getting the menu item list based on current arguments and creates a menu for GUI

options(meals) :-
get_meals(L), 
createDOMV1(L).

options(sauces) :- 
get_sauces(L), 
createDOMV2(L).

options(breads) :- 
get_breads(L), 
createDOMV1(L).

options(meats) :- 
get_meats(L), 
createDOMV2(L).

options(veggies) :- 
get_veggies(L), 
createDOMV1(L).

options(topups) :- 
get_topups(L), 
createDOMV2(L).

options(sides) :- 
get_sides(L), 
createDOMV1(L).

% Declare predicates for asserting facts if given input is not already selected
% only will assert if X is not in the selected list e.g check_selection(X, meals) is false
selected(X,meals) :-
 \\+check_selection(X, meals) -> asserta(selected_meals(X)).

selected(X,breads) :- 
\\+check_selection(X, breads) -> asserta(selected_breads(X)).

selected(X,meats) :- 
\\+check_selection(X, meats) ->asserta(selected_meats(X)).

selected(X,veggies) :- 
\\+check_selection(X, veggies) ->asserta(selected_veggies(X)).

selected(X,sauces) :- 
\\+check_selection(X, sauces) ->asserta(selected_sauces(X)).

selected(X,topups) :-
 \\+check_selection(X, topups) ->asserta(selected_topups(X)).

selected(X,sides) :-
 \\+check_selection(X, sides) ->asserta(selected_sides(X)).

% Declare predicates for checking if X is already in selected list
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

% Declare predicates for for getting the corresponding choices based on user's input
% e.g findall(X, pred(X), List) - Finds possible values for predicate and displays them on the GUI

show_meals(Meals) :- 
findall(X, selected_meals(X), Meals),
 displayOrder('Meal:',Meals).

show_breads(Breads) :-
findall(X, selected_breads(X), Breads), 
displayOrder('Breads: ',Breads).

show_meats(Meats) :- 
findall(X, selected_meats(X), Meats), 
displayOrder('Meats: ',Meats).

show_veggies(Veggies) :- 
findall(X, selected_veggies(X), Veggies), 
displayOrder('Veggies: ',Veggies).

show_sauces(Sauces) :- 
findall(X, selected_sauces(X), Sauces), 
displayOrder('Sauces: ',Sauces).

show_topups(Topups) :- 
findall(X, selected_topups(X), Topups), 
displayOrder('Topups: ',Topups).

show_sides(Sides) :- 
findall(X, selected_sides(X), Sides), 
displayOrder('Sides: ',Sides).

% Declare predicate for displaying all the selected ingredients for the final order on the GUI if input X is 1
displaySelections(X) :- 
(X==1) ->
show_meals(Meals),
show_breads(Breads),
show_meats(Meats),
show_veggies(Veggies),
show_sauces(Sauces), 
show_topups(Topups),
show_sides(Sides).

% Chooses a random behaviour
% Then chooses a random gesture assigned to that selected behaviour
setStaffBehaviour(list) :-
behaviour(List1),
random_member(B1, List1),
behaviour(B1, List2),
random_member(B, List2),
displayStaffGesture(B).

% GUI functions

% Display staff gesture
displayStaffGesture(B) :-
create(a, A),  
    atom_concat('-', B, Y),        
    atom_concat(Y, '- Welcome to Subway, what kind of meal would you like?', Z),                                 
    html(A, Z),                                  
    get_by_id('subway-header', Parent),
    append_child(Parent, A).

% For displaying the final order
% Used to add the selected ingredients to a <a></a> and appends to the GUI
% displayOrder is called recursively until the list is exhausted
displayOrder(X,[]):- % empty list
create(a, A),  
atom_concat(X, 'None.', Y),                                       
    html(A, Y),
create(br, BR),                                     
    get_by_id('subway-contents', Parent),
    append_child(Parent, A),
    append_child(Parent, BR).
    

displayOrder(X,[H]) :- % last item in list
create(a, A),  
atom_concat(H, '.', Y),                                       
    html(A, Y),
create(br, BR),                                     
    get_by_id('subway-contents', Parent),
    append_child(Parent, A),
    append_child(Parent, BR).
    
displayOrder(X,[H|T]) :-  % List with items more than one
create(a, A),  
atom_concat(H, ', ', Y),                                         
    html(A, Y),                                   
    get_by_id('subway-contents', Parent),
    append_child(Parent, A),
displayOrder(X,T), !.  

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
% createDOMV1 and createDOMV2 is called recursively until the list is exhausted
createDOMV1([]). % empty list

createDOMV1([H]) :- % last item in list
createButton(H),
createMenuItems(H).
    
createDOMV1([H|T]) :-  % List with items more than one
createButton(H),
createMenuItems(H),
createDOMV1(T), !.  

% createDOMV2 is the same as createDOMV1 except it's for nested lists

createDOMV2([[]]). % empty list

createDOMV2([[H]]) :- % last item in list
createButton(H),
createMenuItems(H).

% List contains more than 1 item
createDOMV2([[H|T]]) :-  
createButton(H),
createMenuItems(H),
createDOMV2([T]), !.  
`
