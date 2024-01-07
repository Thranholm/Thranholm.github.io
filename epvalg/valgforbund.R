## Valgforbund

valgforbund <- tibble(aar = 2019,
                      bogstav = c("A", "B", "C", "F", "I", "N", "O", "V", "Ø", "Å"),
                      forbund_bogstav = c("A_F", "B_Å", "C_I_V", "A_F", "C_I_V", "N_Ø", "O", "C_I_V", "N_Ø", "B_Å")) %>% 
  bind_rows(tibble(aar = 2014,
                   bogstav = c("A", "B", "C", "F", "I", "N", "O", "V"),
                   forbund_bogstav = c("A_B_F", "A_B_F", "C_V", "A_B_F", "I", "N", "O", "C_V"))) %>% 
  bind_rows(tibble(aar = 2009,
                   bogstav = c("A", "B", "C", "F", "I", "J", "N", "O", "V"),
                   forbund_bogstav = c("A_B_F", "A_B_F", "C_I_V", "A_B_F", "C_I_V", "J_N", "J_N", "O", "C_I_V"))) %>% 
  filter(aar == params$aar) %>% 
  select(-aar)




