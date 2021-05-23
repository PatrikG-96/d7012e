
my_last(X, [X]).        %Base case?
my_last(X, [_|XS]) :- my_last(X, XS).