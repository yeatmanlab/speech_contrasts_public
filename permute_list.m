function newlist = permute_list(list, repeats)

% This function takes a vector of cells (list) and shuffles it into a new
% list. Arg "repeats" says how many times each element of list will be
% presented. 
[rows, ~] = size(list);

big_list = repmat(list,1, repeats);

random_order = randperm(length(big_list));
total_trials = length(random_order);
newlist = {};
for thisStim = 1:total_trials
    for thisRow = 1:rows
    newlist{thisRow,thisStim} = [big_list{thisRow,random_order(thisStim)}];
    end
end


end