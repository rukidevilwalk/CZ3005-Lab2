export default `

:- use_module(library(dom)).
:- use_module(library(js)).
% Set up list methods for appending
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

% print_options is used to print the items based on the given list.
print_options([]). % empty list

print_options([H|T]) :-  % List with items more than one
    write(H), 
    write(', '), 
    print_options(T), !. % remove the item then print it one by one

 buttonClicked(Text) :-
    prop(buttonClicked, ButtonClicked),
    apply(ButtonClicked, [Text], _).

init :-
buttonClicked('veggie').

options(meals) :- meals(L), print_options(L).
options(meals1) :- meals(L).
% selected(X,L) :- .

% Get user corresponding choice
% findall(X, pred(X), List) - Find possible values for predicate and add to the List
show_meals(Meals) :- findall(X, chosen_meals(X), Meals).
show_breads(Breads) :- findall(X, chosen_breads(X), Breads).
show_meats(Meats) :- findall(X, chosen_meats(X), Meats).
show_veggies(Veggies) :- findall(X, chosen_veggies(X), Veggies).
show_sauces(Sauces) :- findall(X, chosen_sauces(X), Sauces).
show_topups(TopUps) :- findall(X, chosen_topups(X), TopUps).
show_sides(Sides) :- findall(X, chosen_sides(X), Sides).
`
