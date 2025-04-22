import re
from datetime import datetime, timedelta

def atualizar_prova_da_semana():
    # Lê o conteúdo atual do README
    with open(r"5_Periodo\Readme.md", "r", encoding="utf-8") as f:
        readme = f.read()

    # Função para extrair os dados dos eventos
    def extrair_eventos(bloco, tipo_evento):
        pattern = r"- (.+?) - Professor[a]?: (.+?) - Data: (\d{2}/\d{2})"
        matches = re.findall(pattern, bloco)
        eventos = []
        for m in matches:
            try:
                data = datetime.strptime(f"{m[2]}/{datetime.now().year}", "%d/%m/%Y")
                eventos.append({
                    "tipo": tipo_evento,
                    "disciplina": m[0].strip(),
                    "professor": m[1].strip(),
                    "data": data
                })
            except ValueError:
                continue
        return eventos

    # Extrai os blocos de eventos
    blocos = {
        "Prova": re.search(r"## 🗓️ Provas:(.*?)(?=##|$)", readme, re.DOTALL).group(1),
        "Trabalho": re.search(r"## 🗓️ Entrega de trabalhos:(.*?)(?=##|$)", readme, re.DOTALL).group(1),
        "Exame": re.search(r"## 🗓️ Provas de Exames:(.*?)(?=##|$)", readme, re.DOTALL).group(1),
    }

    # Processa todos os eventos
    eventos = []
    for tipo, bloco in blocos.items():
        eventos.extend(extrair_eventos(bloco, tipo))

    # Define o período da semana (segunda a sexta)
    hoje = datetime.now()
    if hoje.weekday() >= 5:  
        inicio_semana = hoje + timedelta(days=(7 - hoje.weekday()))  
    else:
        inicio_semana = hoje - timedelta(days=hoje.weekday())  # Segunda da semana atual
    fim_semana = inicio_semana + timedelta(days=6)  

    # Filtra eventos da semana
    eventos_semana = [
        e for e in eventos 
        if inicio_semana.date() <= e["data"].date() <= fim_semana.date()
    ]

    # Ordena eventos por data
    eventos_semana.sort(key=lambda x: x["data"])

    # Gera o texto com todos os eventos da semana
    if eventos_semana:
        texto_prova = "📌 **Eventos da semana:**\n"
        for e in eventos_semana:
            texto_prova += f"- {e['tipo']}: {e['disciplina']} — {e['data'].strftime('%d/%m')} (Professor: {e['professor']})\n"
    else:
        texto_prova = "📌 **Nenhum evento acadêmico marcado para esta semana!**"

    # Atualiza o README
    novo_readme = re.sub(
        r"<!-- PROVA_DA_SEMANA -->.*?<!-- FIM_PROVA_DA_SEMANA -->",
        f"<!-- PROVA_DA_SEMANA -->\n{texto_prova}\n<!-- FIM_PROVA_DA_SEMANA -->",
        readme,
        flags=re.DOTALL
    )

    # Salva as alterações
    with open(r"5_Periodo\Readme.md", "w", encoding="utf-8") as f:
        f.write(novo_readme)

    print("✅ README atualizado com sucesso!")
    return print(texto_prova)

if __name__ == "__main__":
    atualizar_prova_da_semana()