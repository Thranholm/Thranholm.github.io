library(tidyverse)

ant_mandater <- 19

parti_data <- tibble(partier = c("A", "B", "C", "D", "F", "O", "V")) %>% 
  mutate(stemmer = sample(1:879650, length(partier)))

valgforbund <- tibble(partier = c("A", "B", "C", "D", "F", "O", "V"),
                      valgforbund = c("A_B", "A_B", "C_D", "C_D", "F_V", "O", "F_V"))

tabel_data <- tibble(Parti = c("A", "B", "C", "D", "F", "O", "V"),
                     Valgforbund = c("A_B", "A_B", "C_D", "C_D", "F_V", "O", "F_V")) %>% 
  mutate(Stemmer = sample(1:879650, length(Parti)))

dhont <- tibble(dhont = 1:ant_mandater)

valgforbund_kvotienter <- parti_data %>% 
  left_join(valgforbund, by = join_by(partier)) %>% 
  summarise(stemmer = sum(stemmer), .by = valgforbund) %>% 
  cross_join(dhont) %>% 
  mutate(kvotienter = stemmer / dhont,
         mandat = rank(-kvotienter) <= ant_mandater)

valgforbund_mandater <- valgforbund_kvotienter %>% 
  summarise(valgforbund_mandater = sum(mandat), .by = valgforbund)

dhont_forbund <- tibble(dhont = 1:max(valgforbund_mandater$valgforbund_mandater))


parti_kvotienter <- parti_data %>% 
  left_join(valgforbund, by = join_by(partier)) %>% 
  left_join(valgforbund_mandater, by = join_by(valgforbund)) %>% 
  cross_join(dhont_forbund) %>% 
  mutate(kvotienter = stemmer / dhont) %>% 
  mutate(kvotient_rank = rank(-kvotienter), .by = valgforbund) %>% 
  mutate(mandat = kvotient_rank <= valgforbund_mandater)

parti_mandater <- parti_kvotienter %>% 
  summarise(parti_mandater = sum(mandat), .by = c(partier, valgforbund, stemmer))


ingen_valgforbund <- parti_data %>% 
  cross_join(dhont) %>% 
  mutate(kvotient = stemmer / dhont,
         mandat = rank(-kvotient) <= ant_mandater) %>% 
  summarise(ingen_forbund_mandater = sum(mandat), .by = partier)



