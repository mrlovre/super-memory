%% 20 pravila
load('data')

%%
figure, hold on
semilogy(log10(SGD_errors))
semilogy(log10(GD_errors))
legend SGD GD
title 'Mean squared error'
xlabel 'training step'
ylabel 'log error'

%%
figure, hold on
semilogy(log10(SGD_low_train_rate))
semilogy(log10(GD_low_train_rate))
legend SGD GD
title 'Mean squared error'
xlabel 'training step'
ylabel 'log error'

%%
figure, hold on
semilogy(log10(SGD_high_train_rate))
semilogy(log10(GD_high_train_rate))
legend SGD GD
title 'Mean squared error'
xlabel 'training step'
ylabel 'log error'

%%
X = -4 : 0.1 : 4;
Y = -4 : 0.1 : 4;
for i = 1 : 8
    membershipA(i, :) = 1 ./ (1 + exp(-Ab(i) .* (X - Aa(i))));
    membershipB(i, :) = 1 ./ (1 + exp(-Bb(i) .* (X - Ba(i))));
end

%%
figure
for i = 1 : 8
    subplot(2, 4, i), plot(X, membershipA(i, :))
    xlim([-4, 4]), ylim([0, 1])
    title(sprintf('\\mu_A^{(%d)}(x; %.2f, %.2f)', i, Aa(i), Ab(i)))
    grid minor
end

%%
figure
for i = 1 : 8
    subplot(2, 4, i), plot(Y, membershipB(i, :))
    xlim([-4, 4]), ylim([0, 1])
    title(sprintf('\\mu_B^{(%d)}(y; %.2f, %.2f)', i, Ba(i), Bb(i)))
    grid minor
end

%%
[X2, Y2] = meshgrid(X, Y);
figure
for i = 1 : 8
    [mx, my] = meshgrid(membershipA(i, :), membershipB(i, :));
    Z(i, :, :) = mx .* my;
    w = reshape(Z(i, :, :), length(X), length(Y));
    subplot(2, 4, i), surf(X2, Y2, w, 'EdgeColor', 'None'), colormap jet
    xlim([-4, 4]), ylim([-4, 4]), zlim([0, 1]), grid minor, title(sprintf('P_%d', i))
end

%%
figure
for i = 1 : 8
    transfer(i, :, :) = p(i) * X2 + q(i) * Y2 + r(i);
    w = reshape(transfer(i, :, :), length(X), length(Y));
    subplot(2, 4, i), surf(X2, Y2, w, 'EdgeColor', 'None'), colormap jet
    xlim([-4, 4]), ylim([-4, 4]), grid minor, title(sprintf('T_%d(x, y; %.2f, %.2f, %.2f)', i, p(i), q(i), r(i)))    
end

%%
figure
Ztotal = sum(Z, 1);
for i = 1 : 8   
    w = reshape(transfer(i, :, :) .* Z(i, :, :) ./ Ztotal(1, :, :), length(X), length(Y));
    subplot(2, 4, i), surf(X2, Y2, w, 'EdgeColor', 'None'), colormap jet
    xlim([-4, 4]), ylim([-4, 4]), grid minor, title(sprintf('P_%d T_%d', i, i))
end

%%
figure
total(:, :) = reshape(transfer(1, :, :) .* Z(1, :, :) ./ Ztotal(1, :, :), length(X), length(Y));
for i = 2 : 8
    total(:, :) = total(:, :) + reshape(transfer(i, :, :) .* Z(i, :, :) ./ Ztotal(1, :, :), length(X), length(Y));
end
surf(X2, Y2, total, 'EdgeColor', 'None'), colormap jet, title 'Naučena funkcija', grid minor

%%
figure
f = ((X2 - 1) .^ 2 + (Y2 + 2) .^ 2 - 5 * X2 .* Y2 + 3) .* cos(X2./5) .^ 2;
surf(X2, Y2, f, 'EdgeColor', 'None'), colormap jet
title 'Zadana funkcija', grid minor

%%
figure
surf(X2, Y2, (total - f), 'EdgeColor', 'None'), colormap jet
title 'Razlika', grid minor