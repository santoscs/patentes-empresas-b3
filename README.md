Results
-------

### Summary statistics of the variables

    library(skimr)
    ed <- dados %>%
      dplyr::select(pro_patente, liq_cor, liq_geral, end_ger, end_ce, roa, roe, intang_ativo, intang_pl, pagr, pplgr, tamanho, idade, e_cedente, e_cessionaria, alta, baixa) %>%
      skim()



    tab <- ed %>% dplyr::filter(stat=="mean") %>% dplyr::select(variable)
    tab <- cbind(tab, ed %>% dplyr::filter(stat=="mean") %>% dplyr::select(formatted))
    tab <- cbind(tab, ed %>% dplyr::filter(stat=="p50") %>% dplyr::select(formatted))
    tab <- cbind(tab, ed %>% dplyr::filter(stat=="sd") %>% dplyr::select(formatted))
    tab <- cbind(tab, ed %>% dplyr::filter(stat=="p0") %>% dplyr::select(formatted))
    tab <- cbind(tab, ed %>% dplyr::filter(stat=="p100") %>% dplyr::select(formatted))


    colnames(tab) <- c("Variable", "Mean", "S.Dev.", "Min.", "Max.")

    tab[,"Variable"] <- c("Patent Application", "Current Liquidity", "General Liquidity", "Debt Ratio", "Short-term Debt Ratio", "Return on Assets  (ROA)", "Return on Equity (ROE)", "Equity Intangibility", "Asset Intangibility", "Equity's potential to add value",  "Asset's potential to add value", "Size", "Age", "Technology assignor", "Technology assignee", "High technological intensity", "Low technological intensity")

    skimr::kable(tab)

<table>
<thead>
<tr class="header">
<th align="center">Variable</th>
<th align="center">Mean</th>
<th align="center">S.Dev.</th>
<th align="center">Min.</th>
<th align="center">Max.</th>
<th align="center">NA</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">Patent Application</td>
<td align="center">0.11</td>
<td align="center">0</td>
<td align="center">0.31</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">Current Liquidity</td>
<td align="center">37.14</td>
<td align="center">1.36</td>
<td align="center">1146.95</td>
<td align="center">0.0054</td>
<td align="center">39077.09</td>
</tr>
<tr class="odd">
<td align="center">General Liquidity</td>
<td align="center">2.09</td>
<td align="center">1.6</td>
<td align="center">2.26</td>
<td align="center">0.014</td>
<td align="center">31.29</td>
</tr>
<tr class="even">
<td align="center">Debt Ratio</td>
<td align="center">0.45</td>
<td align="center">0.42</td>
<td align="center">0.22</td>
<td align="center">1.3e-07</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">Short-term Debt Ratio</td>
<td align="center">0.86</td>
<td align="center">0.62</td>
<td align="center">2.52</td>
<td align="center">0.032</td>
<td align="center">70.37</td>
</tr>
<tr class="even">
<td align="center">Return on Assets (ROA)</td>
<td align="center">-0.014</td>
<td align="center">0.028</td>
<td align="center">0.49</td>
<td align="center">-11.19</td>
<td align="center">11.89</td>
</tr>
<tr class="odd">
<td align="center">Return on Equity (ROE)</td>
<td align="center">0.14</td>
<td align="center">0.093</td>
<td align="center">2.19</td>
<td align="center">-15.17</td>
<td align="center">75</td>
</tr>
<tr class="even">
<td align="center">Equity Intangibility</td>
<td align="center">0.17</td>
<td align="center">0.047</td>
<td align="center">0.23</td>
<td align="center">0</td>
<td align="center">0.97</td>
</tr>
<tr class="odd">
<td align="center">Asset Intangibility</td>
<td align="center">0.62</td>
<td align="center">0.11</td>
<td align="center">2.08</td>
<td align="center">-25.4</td>
<td align="center">44.56</td>
</tr>
<tr class="even">
<td align="center">Equity's potential to add value</td>
<td align="center">0.31</td>
<td align="center">0.28</td>
<td align="center">0.48</td>
<td align="center">-7.18</td>
<td align="center">15.68</td>
</tr>
<tr class="odd">
<td align="center">Asset's potential to add value</td>
<td align="center">1.03</td>
<td align="center">0.63</td>
<td align="center">10.81</td>
<td align="center">-369.2</td>
<td align="center">233.5</td>
</tr>
<tr class="even">
<td align="center">Size</td>
<td align="center">14.38</td>
<td align="center">14.59</td>
<td align="center">2.09</td>
<td align="center">3.25</td>
<td align="center">20.62</td>
</tr>
<tr class="odd">
<td align="center">Age</td>
<td align="center">35.71</td>
<td align="center">31</td>
<td align="center">25.84</td>
<td align="center">0</td>
<td align="center">126</td>
</tr>
<tr class="even">
<td align="center">Technology assignor</td>
<td align="center">0.085</td>
<td align="center">0</td>
<td align="center">0.28</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">Technology assignee</td>
<td align="center">0.37</td>
<td align="center">0</td>
<td align="center">0.48</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">High technological intensity</td>
<td align="center">0.11</td>
<td align="center">0</td>
<td align="center">0.31</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">Low technological intensity</td>
<td align="center">0.21</td>
<td align="center">0</td>
<td align="center">0.41</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
</tbody>
</table>

### Distribution of companies and propensity to patent by sectors

    c1 <- dados %>%
      count(setor, cnpj) %>%
      count(setor) %>%
      mutate(empresas=nn)  %>%
      select(-nn) 
    c2 <- dados %>%
      count(setor, pro_patente, cnpj) %>%
      count(setor, pro_patente) %>%
      filter(pro_patente==1) %>%
      mutate(empresas_patentes=nn) %>%
      select(-pro_patente, -nn) 
    tab <- left_join(c1, c2)

    ## Joining, by = "setor"

    c3 <- tab %>%
      mutate(propensao_patente = empresas_patentes/empresas) %>%
      mutate(propensao_patente=round_half_up(propensao_patente, digits = 2)) %>%
      select(-empresas_patentes, -empresas)

    tab <- tab %>%
      adorn_totals("row") %>%
      adorn_percentages("all") %>%
      adorn_pct_formatting(affix_sign = FALSE) %>%
      adorn_ns(position = "front") %>%
      as.data.frame()


    tab <- left_join(tab, c3)

    ## Joining, by = "setor"

    ## Warning: Column `setor` joining character vector and factor, coercing into
    ## character vector

    colnames(tab) <- c('Sector', 'Num. of companies (% of total)', 'Num. of companies with one or more patents (% of total)', 'Propensity to patent' )

    tab <- rbind(c(" ", "A", "B", "C= B/A"), tab)
    knitr::kable(tab)  

<table>
<thead>
<tr class="header">
<th align="left">Sector</th>
<th align="left">Num. of companies (% of total)</th>
<th align="left">Num. of companies with one or more patents (% of total)</th>
<th align="left">Propensity to patent</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"></td>
<td align="left">A</td>
<td align="left">B</td>
<td align="left">C= B/A</td>
</tr>
<tr class="even">
<td align="left">Bens Industriais</td>
<td align="left">71 (17.7)</td>
<td align="left">14 (3.5)</td>
<td align="left">0.2</td>
</tr>
<tr class="odd">
<td align="left">Consumo Cíclico</td>
<td align="left">80 (20.0)</td>
<td align="left">6 (1.5)</td>
<td align="left">0.08</td>
</tr>
<tr class="even">
<td align="left">Consumo não Cíclico</td>
<td align="left">25 (6.2)</td>
<td align="left">7 (1.7)</td>
<td align="left">0.28</td>
</tr>
<tr class="odd">
<td align="left">Materiais Básicos</td>
<td align="left">31 (7.7)</td>
<td align="left">12 (3.0)</td>
<td align="left">0.39</td>
</tr>
<tr class="even">
<td align="left">Não Classificados</td>
<td align="left">16 (4.0)</td>
<td align="left">NA (-)</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Petróleo. Gás e Biocombustíveis</td>
<td align="left">11 (2.7)</td>
<td align="left">1 (0.2)</td>
<td align="left">0.09</td>
</tr>
<tr class="even">
<td align="left">Saúde</td>
<td align="left">19 (4.7)</td>
<td align="left">3 (0.7)</td>
<td align="left">0.16</td>
</tr>
<tr class="odd">
<td align="left">Tecnologia da Informação</td>
<td align="left">7 (1.7)</td>
<td align="left">3 (0.7)</td>
<td align="left">0.43</td>
</tr>
<tr class="even">
<td align="left">Telecomunicações</td>
<td align="left">5 (1.2)</td>
<td align="left">NA (-)</td>
<td align="left">NA</td>
</tr>
<tr class="odd">
<td align="left">Utilidade Pública</td>
<td align="left">63 (15.7)</td>
<td align="left">27 (6.7)</td>
<td align="left">0.43</td>
</tr>
<tr class="even">
<td align="left">Total</td>
<td align="left">328 (81.8)</td>
<td align="left">73 (18.2)</td>
<td align="left">NA</td>
</tr>
</tbody>
</table>

### Distribuição das empresas da B3 e suas patentes por ano

    c1 <- dados %>%
      count(ano, cnpj) %>%
      count(ano) %>%
      mutate(empresas=nn) %>%
      select(-nn) 

    c2 <- dados %>%
      count(ano, pro_patente, cnpj) %>%
      count(ano, pro_patente) %>%
      filter(pro_patente==1) %>%
      mutate(empresas_patentes=nn) %>%
      select(-pro_patente, -nn) 

    tab <- left_join(c1, c2)

    ## Joining, by = "ano"

    tab <- tab %>%
      mutate(propensao_patente = empresas_patentes/empresas) %>%
      mutate(propensao_patente=round_half_up(propensao_patente, digits = 2))

    colnames(tab) <- c('Ano', 'Numero de empresas', 'Numero de empresas com 1 patente ou mais', 'Propensao a patentear' )
    tab <- rbind(c(" ", "A", "B", "C= B/A"), tab)
    knitr::kable(tab)  

<table>
<thead>
<tr class="header">
<th align="left">Ano</th>
<th align="left">Numero de empresas</th>
<th align="left">Numero de empresas com 1 patente ou mais</th>
<th align="left">Propensao a patentear</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"></td>
<td align="left">A</td>
<td align="left">B</td>
<td align="left">C= B/A</td>
</tr>
<tr class="even">
<td align="left">2010</td>
<td align="left">251</td>
<td align="left">31</td>
<td align="left">0.12</td>
</tr>
<tr class="odd">
<td align="left">2011</td>
<td align="left">270</td>
<td align="left">22</td>
<td align="left">0.08</td>
</tr>
<tr class="even">
<td align="left">2012</td>
<td align="left">274</td>
<td align="left">25</td>
<td align="left">0.09</td>
</tr>
<tr class="odd">
<td align="left">2013</td>
<td align="left">281</td>
<td align="left">39</td>
<td align="left">0.14</td>
</tr>
<tr class="even">
<td align="left">2014</td>
<td align="left">286</td>
<td align="left">37</td>
<td align="left">0.13</td>
</tr>
<tr class="odd">
<td align="left">2015</td>
<td align="left">292</td>
<td align="left">30</td>
<td align="left">0.1</td>
</tr>
<tr class="even">
<td align="left">2016</td>
<td align="left">305</td>
<td align="left">32</td>
<td align="left">0.1</td>
</tr>
<tr class="odd">
<td align="left">2017</td>
<td align="left">308</td>
<td align="left">25</td>
<td align="left">0.08</td>
</tr>
</tbody>
</table>

### Logit model pooled

Sem setor

    library(margins)
    library(lmtest)

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

    pooled.logit <- glm(pro_patente ~ liq_cor + liq_geral + end_ger + end_ce + roa + roe + intang_ativo + intang_pl + pagr + pplgr + tamanho + idade + e_cedente + e_cessionaria + alta + baixa, 
                        family=binomial(link="logit"), data=dados)

    tab <- rtble.glm(pooled.logit)

    ## Joining, by = "factor"

    tab %>% 
      mutate(factor = c("intercept", "Current Liquidity", "General Liquidity", "Debt Ratio", "Short-term Debt Ratio", "Return on Assets  (ROA)", "Return on Equity (ROE)", "Equity Intangibility", "Asset Intangibility", "Equity's potential to add value",  "Asset's potential to add value", "Size", "Age", "Technology assignor", "Technology assignee", "High technological intensity", "Low technological intensity", "AIC", "observations", "McFadden Pseudo R2", "Effron Pseudo R2", "Nagelkerke Pseudo R2", "predictedy1",  "predictedy0", "predictedy1and0")) %>%
      knitr::kable(., digits = 3)

<table>
<thead>
<tr class="header">
<th align="left">factor</th>
<th align="right">Estimate</th>
<th align="right">Std. Error</th>
<th align="right">Pr(&gt;|z|)</th>
<th align="right">AME</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">intercept</td>
<td align="right">-5.532</td>
<td align="right">1.886</td>
<td align="right">0.003</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">Current Liquidity</td>
<td align="right">-0.079</td>
<td align="right">0.111</td>
<td align="right">0.475</td>
<td align="right">-0.006</td>
</tr>
<tr class="odd">
<td align="left">General Liquidity</td>
<td align="right">-1.181</td>
<td align="right">0.418</td>
<td align="right">0.005</td>
<td align="right">-0.086</td>
</tr>
<tr class="even">
<td align="left">Debt Ratio</td>
<td align="right">-2.982</td>
<td align="right">0.601</td>
<td align="right">0.000</td>
<td align="right">-0.217</td>
</tr>
<tr class="odd">
<td align="left">Short-term Debt Ratio</td>
<td align="right">-4.863</td>
<td align="right">1.375</td>
<td align="right">0.000</td>
<td align="right">-0.353</td>
</tr>
<tr class="even">
<td align="left">Return on Assets (ROA)</td>
<td align="right">1.170</td>
<td align="right">1.146</td>
<td align="right">0.307</td>
<td align="right">0.085</td>
</tr>
<tr class="odd">
<td align="left">Return on Equity (ROE)</td>
<td align="right">-0.004</td>
<td align="right">0.148</td>
<td align="right">0.981</td>
<td align="right">0.000</td>
</tr>
<tr class="even">
<td align="left">Equity Intangibility</td>
<td align="right">0.435</td>
<td align="right">0.498</td>
<td align="right">0.383</td>
<td align="right">0.032</td>
</tr>
<tr class="odd">
<td align="left">Asset Intangibility</td>
<td align="right">-0.094</td>
<td align="right">0.101</td>
<td align="right">0.355</td>
<td align="right">-0.007</td>
</tr>
<tr class="even">
<td align="left">Equity's potential to add value</td>
<td align="right">1.886</td>
<td align="right">0.410</td>
<td align="right">0.000</td>
<td align="right">0.137</td>
</tr>
<tr class="odd">
<td align="left">Asset's potential to add value</td>
<td align="right">-0.003</td>
<td align="right">0.022</td>
<td align="right">0.874</td>
<td align="right">0.000</td>
</tr>
<tr class="even">
<td align="left">Size</td>
<td align="right">0.540</td>
<td align="right">0.059</td>
<td align="right">0.000</td>
<td align="right">0.039</td>
</tr>
<tr class="odd">
<td align="left">Age</td>
<td align="right">-0.002</td>
<td align="right">0.004</td>
<td align="right">0.489</td>
<td align="right">0.000</td>
</tr>
<tr class="even">
<td align="left">Technology assignor</td>
<td align="right">0.408</td>
<td align="right">0.231</td>
<td align="right">0.077</td>
<td align="right">0.030</td>
</tr>
<tr class="odd">
<td align="left">Technology assignee</td>
<td align="right">0.911</td>
<td align="right">0.173</td>
<td align="right">0.000</td>
<td align="right">0.066</td>
</tr>
<tr class="even">
<td align="left">High technological intensity</td>
<td align="right">2.465</td>
<td align="right">0.262</td>
<td align="right">0.000</td>
<td align="right">0.179</td>
</tr>
<tr class="odd">
<td align="left">Low technological intensity</td>
<td align="right">0.689</td>
<td align="right">0.218</td>
<td align="right">0.002</td>
<td align="right">0.050</td>
</tr>
<tr class="even">
<td align="left">AIC</td>
<td align="right">1143.069</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">observations</td>
<td align="right">2269.000</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">McFadden Pseudo R2</td>
<td align="right">0.278</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">Effron Pseudo R2</td>
<td align="right">0.208</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">Nagelkerke Pseudo R2</td>
<td align="right">0.349</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">predictedy1</td>
<td align="right">56.627</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">predictedy0</td>
<td align="right">91.129</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">predictedy1and0</td>
<td align="right">89.868</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
</tbody>
</table>

Com setor

    pooled.logit <- glm(pro_patente ~ liq_cor + liq_geral + end_ger + end_ce + roa + roe + intang_ativo + intang_pl + pagr + pplgr + tamanho + idade + e_cedente + e_cessionaria + alta + baixa + setor,
                        family=binomial(link="logit"), data=dados)

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    tab <- rtble.glm(pooled.logit)

    ## Joining, by = "factor"

    ## Warning: Column `factor` joining factor and character vector, coercing into
    ## character vector

    knitr::kable(tab, digits = 3)

<table>
<thead>
<tr class="header">
<th align="left">factor</th>
<th align="right">Estimate</th>
<th align="right">Std. Error</th>
<th align="right">Pr(&gt;|z|)</th>
<th align="right">AME</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">(Intercept)</td>
<td align="right">-6.613</td>
<td align="right">2.015</td>
<td align="right">0.001</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">liq_cor</td>
<td align="right">-0.035</td>
<td align="right">0.119</td>
<td align="right">0.770</td>
<td align="right">-0.002</td>
</tr>
<tr class="odd">
<td align="left">liq_geral</td>
<td align="right">-0.973</td>
<td align="right">0.413</td>
<td align="right">0.018</td>
<td align="right">-0.066</td>
</tr>
<tr class="even">
<td align="left">end_ger</td>
<td align="right">-2.198</td>
<td align="right">0.724</td>
<td align="right">0.002</td>
<td align="right">-0.150</td>
</tr>
<tr class="odd">
<td align="left">end_ce</td>
<td align="right">-4.221</td>
<td align="right">1.388</td>
<td align="right">0.002</td>
<td align="right">-0.287</td>
</tr>
<tr class="even">
<td align="left">roa</td>
<td align="right">1.109</td>
<td align="right">1.558</td>
<td align="right">0.477</td>
<td align="right">0.075</td>
</tr>
<tr class="odd">
<td align="left">roe</td>
<td align="right">0.012</td>
<td align="right">0.172</td>
<td align="right">0.946</td>
<td align="right">0.001</td>
</tr>
<tr class="even">
<td align="left">intang_ativo</td>
<td align="right">0.104</td>
<td align="right">0.556</td>
<td align="right">0.852</td>
<td align="right">0.007</td>
</tr>
<tr class="odd">
<td align="left">intang_pl</td>
<td align="right">-0.072</td>
<td align="right">0.116</td>
<td align="right">0.538</td>
<td align="right">-0.005</td>
</tr>
<tr class="even">
<td align="left">pagr</td>
<td align="right">2.036</td>
<td align="right">0.478</td>
<td align="right">0.000</td>
<td align="right">0.139</td>
</tr>
<tr class="odd">
<td align="left">pplgr</td>
<td align="right">-0.002</td>
<td align="right">0.022</td>
<td align="right">0.910</td>
<td align="right">0.000</td>
</tr>
<tr class="even">
<td align="left">tamanho</td>
<td align="right">0.516</td>
<td align="right">0.067</td>
<td align="right">0.000</td>
<td align="right">0.035</td>
</tr>
<tr class="odd">
<td align="left">idade</td>
<td align="right">-0.008</td>
<td align="right">0.004</td>
<td align="right">0.037</td>
<td align="right">-0.001</td>
</tr>
<tr class="even">
<td align="left">e_cedente</td>
<td align="right">1.096</td>
<td align="right">0.262</td>
<td align="right">0.000</td>
<td align="right">0.075</td>
</tr>
<tr class="odd">
<td align="left">e_cessionaria</td>
<td align="right">0.982</td>
<td align="right">0.179</td>
<td align="right">0.000</td>
<td align="right">0.067</td>
</tr>
<tr class="even">
<td align="left">alta</td>
<td align="right">2.814</td>
<td align="right">0.331</td>
<td align="right">0.000</td>
<td align="right">0.192</td>
</tr>
<tr class="odd">
<td align="left">baixa</td>
<td align="right">1.054</td>
<td align="right">0.357</td>
<td align="right">0.003</td>
<td align="right">0.072</td>
</tr>
<tr class="even">
<td align="left">setorConsumo Cíclico</td>
<td align="right">-1.512</td>
<td align="right">0.434</td>
<td align="right">0.000</td>
<td align="right">-0.063</td>
</tr>
<tr class="odd">
<td align="left">setorConsumo não Cíclico</td>
<td align="right">0.297</td>
<td align="right">0.429</td>
<td align="right">0.489</td>
<td align="right">0.020</td>
</tr>
<tr class="even">
<td align="left">setorMateriais Básicos</td>
<td align="right">0.570</td>
<td align="right">0.353</td>
<td align="right">0.106</td>
<td align="right">0.041</td>
</tr>
<tr class="odd">
<td align="left">setorNão Classificados</td>
<td align="right">-13.268</td>
<td align="right">409.638</td>
<td align="right">0.974</td>
<td align="right">-0.093</td>
</tr>
<tr class="even">
<td align="left">setorPetróleo. Gás e Biocombustíveis</td>
<td align="right">-1.507</td>
<td align="right">0.691</td>
<td align="right">0.029</td>
<td align="right">-0.062</td>
</tr>
<tr class="odd">
<td align="left">setorSaúde</td>
<td align="right">0.233</td>
<td align="right">0.505</td>
<td align="right">0.644</td>
<td align="right">0.015</td>
</tr>
<tr class="even">
<td align="left">setorTecnologia da Informação</td>
<td align="right">-0.743</td>
<td align="right">0.685</td>
<td align="right">0.278</td>
<td align="right">-0.038</td>
</tr>
<tr class="odd">
<td align="left">setorTelecomunicações</td>
<td align="right">-17.587</td>
<td align="right">1017.510</td>
<td align="right">0.986</td>
<td align="right">-0.093</td>
</tr>
<tr class="even">
<td align="left">setorUtilidade Pública</td>
<td align="right">1.087</td>
<td align="right">0.281</td>
<td align="right">0.000</td>
<td align="right">0.088</td>
</tr>
<tr class="odd">
<td align="left">aic</td>
<td align="right">1064.759</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">n</td>
<td align="right">2269.000</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">McFadden</td>
<td align="right">0.341</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">Effron</td>
<td align="right">0.260</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">Nagelkerke</td>
<td align="right">0.419</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">predictedy1</td>
<td align="right">58.333</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="odd">
<td align="left">predictedy0</td>
<td align="right">91.767</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
<tr class="even">
<td align="left">predictedy</td>
<td align="right">90.176</td>
<td align="right">NA</td>
<td align="right">NA</td>
<td align="right">NA</td>
</tr>
</tbody>
</table>
