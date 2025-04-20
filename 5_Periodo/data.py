import pandas as pd
import re
from datetime import datetime, timedelta

readme = """
## 🗓️ Provas:
 - Estatistica Computacional - Professor: Paulo Jus. - Data: 11/04
 - Controle Estatístico de Qualidade - Professor: Jorge Festa - Data: 22/04
 - Estatistica e planejamento de experimento. - Professora: Fernanda - Data: 29/04
 - Modelo de Regressão Linear - Professor: César - Data: 05/05
 - Controle Estatístico de Qualidade - Professor: Jorge Festa - Data: 29/05
 - Modelo de Regressão Linear - Professor: César - Data: 23/06
 - Estatistica e planejamento de experimento. - Professora: Fernanda - Data: 24/06
 - Controle Estatístico de Qualidade - Professor: Jorge Festa - Data: 26/06

## 🗓️ Entrega de trabalhos:
 - Estatística Não Paramétrica - Professor: Lucambio - Data: 14/04
 - Estatistica e planejamento de experimento. - Professora: Fernanda - Data: 19/05

## 🗓️ Provas de Exames:
 - Controle Estatístico de Qualidade - Professor: Jorge Festa - Data: 01/07
 - Modelo de Regressão Linear - Professor: César - Data: 07/07
"""

# Função para extrair os dados
def extrair_eventos(bloco, tipo_evento):
    pattern = r"- (.+?) - Professor[a]?: (.+?) - Data: (\d{2}/\d{2})"
    matches = re.findall(pattern, bloco)
    return [
        {
            "tipo_evento": tipo_evento,
            "disciplina": m[0].strip(),
            "professor": m[1].strip(),
            "data": datetime.strptime(f"{m[2]}/2025", "%d/%m/%Y")
        }
        for m in matches
    ]

# Divide os blocos por tipo
blocos = {
    "Prova": re.search(r"## 🗓️ Provas:(.*?)(?=##|$)", readme, re.DOTALL).group(1),
    "Trabalho": re.search(r"## 🗓️ Entrega de trabalhos:(.*?)(?=##|$)", readme, re.DOTALL).group(1),
    "Exame": re.search(r"## 🗓️ Provas de Exames:(.*?)(?=##|$)", readme, re.DOTALL).group(1),
}

# Extrai e concatena todos os eventos
eventos = []
for tipo, bloco in blocos.items():
    eventos.extend(extrair_eventos(bloco, tipo))

# Cria o DataFrame
df_eventos = pd.DataFrame(eventos).sort_values("data").reset_index(drop=True)




hoje = datetime.now()
if hoje.weekday() >= 5:  
    inicio_semana = hoje + timedelta(days=(7 - hoje.weekday())) 
else:
    inicio_semana = hoje - timedelta(days=hoje.weekday()) 
fim_semana = inicio_semana + timedelta(days=5)         

df_semana = df_eventos[(df_eventos["data"] >= inicio_semana) & (df_eventos["data"] <= fim_semana)]




# Gera o texto
if not df_semana.empty:
    prova = df_semana.sort_values("data").iloc[0]
    texto_prova = f"📌 **Prova da semana:** {prova['disciplina']} — {prova['data'].strftime('%d/%m')} (Professor: {prova['professor']})"
else:
    texto_prova = "📌 **Prova da semana:** Nenhuma prova marcada para esta semana!"

print(texto_prova)
if "<!-- PROVA_DA_SEMANA -->" in readme:
    novo_readme = re.sub(
        r"<!-- PROVA_DA_SEMANA -->.*?<!-- FIM_PROVA_DA_SEMANA -->",
        f"<!-- PROVA_DA_SEMANA -->\n{texto_prova}\n<!-- FIM_PROVA_DA_SEMANA -->",
        readme,
        flags=re.DOTALL
    )
else:
    # Se o marcador não existir, adiciona ao final
    novo_readme = readme.strip() + f"\n\n<!-- PROVA_DA_SEMANA -->\n{texto_prova}\n<!-- FIM_PROVA_DA_SEMANA -->"

# Salva de volta
with open("README.md", "w", encoding="utf-8") as f:
    f.write(novo_readme)

print("✅ README atualizado com a prova da semana!")









