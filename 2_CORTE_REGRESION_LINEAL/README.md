

Estimamos la ecuacion: $\widehat{y_{i}} = \widehat{\beta_{0}} + \widehat{\beta_{1}} \cdot x_{i} + \varepsilon_{1}$

$y_{i} =$ Tasa.de.ahorro

$x_{i}$ = Tasa.de.interes

$x_2$ = Tasa.de.interes^2

$x_XPRO$ = (Tasa.de.interes-mean(Tasa.de.interes))

$y_YPRO$ = (Tasa.de.ahorro-mean(Tasa.de.ahorro))

$x_XPRO_y_YPRO$ = (Tasa.de.interes-mean(Tasa.de.interes))*(Tasa.de.ahorro-mean(Tasa.de.ahorro))

$x_XPRO_2$ = ((Tasa.de.interes-mean(Tasa.de.interes))^2)

$y_EST$ = (2.183 + (1.717 * Tasa.de.interes))

$ERROR$ = (Tasa.de.ahorro-y_EST)

$ERROR_2$ = ERROR^2

$SCR_i$ = (y_EST - mean(DATA$Tasa.de.ahorro))^2

$SEC_i$ = (Tasa.de.ahorro-y_EST)^2

$SCT_i$ = (Tasa.de.ahorro-mean(DATA$Tasa.de.ahorro))^2

$Beta_{1} estimado$ = (sum(DATA$x_XPRO_y_YPRO)/sum(DATA$x_XPRO_2))

$Beta_{0}) estimado$ = mean(DATA$Tasa.de.ahorro) - (Beta_1_estimado*mean(DATA$Tasa.de.interes))

$R_2$ = (1-(sum(DATA$SEC_i)/sum(DATA$SCT_i)))

$varianza ERROR$ = (sum(DATA$ERROR_2)/(20-2))

$D Estandar ERROR$ = sqrt(2.551858)

$\widehat{y_{i}} = 2.1833 + 1.7167 * Tasa.de.interes $


