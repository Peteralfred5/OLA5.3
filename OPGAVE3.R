install.packages("ordinal")

library(readr)
library(stringr)
library(ordinal)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)


# Alt lavet af Andreas
#### OPG 3.1 ####

df <- read.csv("regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv", 
               sep = ";", 
               header = TRUE, 
               fileEncoding = "ISO-8859-1")


df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- 
  str_replace(df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål., "Dårlige", "Dårlig")

freq$Category <- ifelse(
  freq$Var1 %in% c("Gode", "Meget gode"), "Positiv",
  ifelse(freq$Var1 %in% c("Dårlig", "Meget dårlige"), "Negativ", "Neutrale")
)

# Summarize the results by Category
freq_summary <- aggregate(cbind(Freq, Perc) ~ Category, data = freq, sum)

# Add a percentage column for labels
freq$Perc <- round((freq$Freq / sum(freq$Freq)) * 100, 1)

# Create the bar plot
ggplot(freq, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Plot of Responses", x = "Categories", y = "Frequency", fill = "Categories") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

#### OPG 3.2 ####

# ændre navn på kolone i df
colnames(df)[colnames(df) == "Branchekode.primær"] <- "BRANCHEKODE"

# indlæs branchekoder fra dkstat
dkstatbrancher <- read_excel("Dansk-Branchekode-2007-(DB07)-v3-2014.xlsx")

#tilsæt 0 på de koder med kun 5 cifre
df$BRANCHEKODE <- gsub("^([0-9]{5})$", "0\\1", df$BRANCHEKODE)

# Merge vores dataset med dkstat df
df <- merge(dkstatbrancher, df, by = "BRANCHEKODE")

# Slette irrelvante koloner fra den merged df fra dkstat
df[ ,2:10] <- NULL

# Lave de 10 branche kategorier om til factor
df$GRP10KODE <- as.vector(df$GRP10KODE)


# Give en svarscore fra 1 til 5 
df$svarscore <- ifelse(df$`Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` == "Meget dårlige", -1,
                                        ifelse(df$`Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` == "Dårlig", -0.5,
                                               ifelse(df$`Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` == "Neutrale", 0,
                                                      ifelse(df$`Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` == "Gode", 0.5,
                                                             ifelse(df$`Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` == "Meget gode", 1, NA)))))


# brutto & ebitda clm
colnames(df)[28] <- "Bruttofortjeneste_2020"
colnames(df)[40] <- "EBITDA_2020"

forsøg1 <- lm(Bruttofortjeneste_2020 ~ EBITDA_2020, data = df)
summary(forsøg1)

# Ensure svarscore is numeric
df$svarscore <- as.numeric(df$svarscore)

# Forkortet mapping af GRP10KODE til branch labels
branch_labels <- c(
  "1" = "Landbrug og fiskeri",
  "2" = "Industri og forsyning",
  "3" = "Bygge og anlæg",
  "4" = "Handel og transport",
  "5" = "Information og kommunikation",
  "7" = "Ejendomshandel",
  "8" = "Erhvervsservice",
  "9" = "Off. adm., undervisning, sundhed",
  "10" = "Kultur og fritid"
)

avg_data <- df %>%
  mutate(
    svarscore = as.numeric(svarscore)  # Sørg for, at svarscore er numerisk
  ) %>%
  group_by(GRP10KODE) %>%
  summarise(
    avg_svarscore = mean(svarscore, na.rm = TRUE)  # Beregn gennemsnit af svarscore
  ) %>%
  arrange(avg_svarscore)  # Sortér efter stigende gennemsnit

# Udskift GRP10KODE-numre med forkortede branch labels
avg_data$GRP10KODE <- factor(avg_data$GRP10KODE, levels = avg_data$GRP10KODE, labels = branch_labels[as.character(avg_data$GRP10KODE)])

# Lav bar plot
ggplot(avg_data, aes(x = GRP10KODE, y = avg_svarscore, fill = GRP10KODE)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#002E6D", "#FBB040", "#6310F4", "#808996", "#A3DFD0", "#B892F9", "#FAC7F1", "#B4C6EB", "#77CBDA")) +
  labs(
    title = "Hvordan brancher påvirker lånemuligheder for virksomheder",
    x = "Branche",
    y = "Nettotal",
    fill = "Branche"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Roter labels for bedre læsbarhed

# her ser vi at der er tre brancher dom dominær datasættet og derfor er det svært at konkludere
# noget baseret på brancherne
for (i in 1) {
  brancher <- table(df$GRP10KODE)
  print(brancher)
}


# plot med kun de 3 brancher som har flere en 20 virksomheder
# Filtrer kun de ønskede brancher og beregn gennemsnittet
filtered_data <- df %>%
  mutate(svarscore = as.numeric(svarscore)) %>%  # Sørg for, at svarscore er numerisk
  group_by(GRP10KODE) %>%
  summarise(avg_svarscore = mean(svarscore, na.rm = TRUE)) %>%  # Beregn gennemsnit
  filter(GRP10KODE %in% c("2", "3", "4")) %>%  # Kun brancherne Industri og forsyning, Bygge og anlæg, Handel og transport
  arrange(avg_svarscore)  # Sortér efter stigende gennemsnit

# Udskift GRP10KODE-numre med branch labels
filtered_data$GRP10KODE <- factor(
  filtered_data$GRP10KODE,
  levels = filtered_data$GRP10KODE,
  labels = branch_labels[as.character(filtered_data$GRP10KODE)]
)

# Lav bar plot med tilpassede farver og tykkelse
ggplot(filtered_data, aes(x = GRP10KODE, y = avg_svarscore, fill = GRP10KODE)) +
  geom_bar(stat = "identity", color = "black", size = 0, width = 0.6) +  # Tilføj kantfarve og tykkelse
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) +     # Tilpassede farver
  labs(
    title = "Brancher har stor betydning for finansieringsmuligheder",
    x = "Branche",
    y = "Nettotal",
    fill = "Branche"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Roter labels for bedre læsbarhed
    text = element_text(size = 12),                    # Generel tekststørrelse
    plot.title = element_text(hjust = 0.5)  # Centrer titel og gør den fed
  )


#### PLOT TIL BALANCE ####

balanceplot <- as.data.frame(df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål., df$Balance.2020..1.000.kr.)

balanceplot$balance <- rownames(balanceplot)

rownames(balanceplot) <- NULL

balanceplot <- balanceplot[!apply(balanceplot, 1, function(row) any(grepl("Ved ikke", row, ignore.case = TRUE))), ]

balanceplot$balance <- as.numeric(balanceplot$balance)

balanceplot <- balanceplot
balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` <- gsub("Dårlig", "Negativ", balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.`)
balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` <- gsub("Meget dårlige", "Negativ", balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.`)
balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` <- gsub("Gode", "Positiv", balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.`)
balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.` <- gsub("Meget gode", "Positiv", balanceplot$`df$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.`)
#FØR 4484 - EFTER 4433 = 51 NA

# Ensure all combinations of balance categories and sentiments are present
all_combinations <- expand.grid(
  balance_category = c("0-49,999", "50,000-99,999", "100,000+"),
  sentiment = c("Negativ", "Neutrale", "Positiv")
)

# Merge with the percentage data to fill in missing combinations with 0 percentage
percentage_data_full <- merge(
  all_combinations,
  percentage_data,
  by = c("balance_category", "sentiment"),
  all.x = TRUE
)

# Replace NA percentages with 0
percentage_data_full$percentage[is.na(percentage_data_full$percentage)] <- 0

# Plot with consistent bar widths
ggplot(percentage_data_full, aes(x = balance_category, y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +  # Ensure consistent bar width
  scale_fill_manual(
    values = c("Negativ" = "navy", "Neutrale" = "blue", "Positiv" = "lightblue")
  ) +
  labs(
    title = "Finansieringsmuligheder forbedres med øget balance",
    x = "Balance Category",
    y = "Percentage",
    fill = "Sentiment"
  ) +
  theme_minimal()






