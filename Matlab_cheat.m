%%%%%MATLAB CHEAT_Sheet

%%for looping
for (size_t i = 0; i < count; i++) {
  /* code */
}


function f = fibo(n)
if ( ~isscalar(n) || n < 1 || n ~= fix(n))
    error('n must be a positive integer!');
end

f(1) = 1;
f(2) = 1;
for ii = 3:n
    f(ii) = f(ii-2) + f(ii-1);
end


function loop_test(N)
total = 0;
for n = 1:N
    n
    n = n + 1;
    total = total + n;
end
fprintf('Total: %d\n',total);


function [n total] = possum(limit)
total = 0;
n = 0;
while total <= limit
    n = n + 1;
    total = total + n;
end
fprintf('sum: %d count: %d\n', total, n);




%% Examples of skipping remainder of iterations

%% Example 1
% Skipping is accomplished in the while condition.
ii = 1;
while ii < length(readings) && readings(ii) <= 100
    readings(ii) = 0;
    ii = ii + 1;
end

%% Example 2
% Skipping is acomplished with a break statement.
for ii = 1:length(readings)
    if readings(ii) > 100
        break;
    else
        readings(ii) = 0;
    end
end

%% Example 3
% Shows that for-loop index retains is last value
% after the loop is exited. It's similar to Example 2,
% but omits the setting of values to 0 and prints the
% index of the first reading above 100.
for ii = 1:length(readings)
    if readings(ii) > 100
        break;
    end
end
fprintf('First reading above 100 is at index %d.\n', ii);




function s = saddle(M)
[r, c] = size(M);
% Initialize the saddle points to an empty array
s = [];
% Check the dimensions to see if input is a row or column vector
if r > 1
    cols = min(M);          % find the min value in each column if more than 1 row
else
    cols = M;               % vector is a special case, min would give a single value
end
if c > 1
    rows = max(M');         % find the max value in each row
else
    rows = M;               % vector is a special case, max would give a single value
end
for ii = 1:c                % visit each column
    for jj = 1:r            % and each row, that is, each element of M
        if M(jj,ii) == cols(ii) && M(jj,ii) == rows(jj) % if both conditions hold
            s = [s; jj ii];                             % saddle point! Let's add it!
        end
    end
end
