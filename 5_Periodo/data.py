import re
from datetime import datetime, timedelta

def atualizar_prova_da_semana():
    # Lê o conteúdo atual do README
    with open("README.md", "r", encoding="utf-8") as f:
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
    if hoje.weekday() >= 5:  # Sábado ou domingo
        inicio_semana = hoje + timedelta(days=(7 - hoje.weekday()))  # Próxima segunda
    else:
        inicio_semana = hoje - timedelta(days=hoje.weekday())  # Segunda da semana atual
    fim_semana = inicio_semana + timedelta(days=4)  # Sexta-feira

    # Filtra eventos da semana
    eventos_semana = [
        e for e in eventos 
        if inicio_semana.date() <= e["data"].date() <= fim_semana.date()
    ]

    # Ordena por data e pega o primeiro evento (mais próximo)
    eventos_semana.sort(key=lambda x: x["data"])
    proximo_evento = eventos_semana[0] if eventos_semana else None

    # Gera o texto para a prova da semana
    if proximo_evento:
        texto_prova = (
            f"📌 **{proximo_evento['tipo']} da semana:** {proximo_evento['disciplina']} — "
            f"{proximo_evento['data'].strftime('%d/%m')} (Professor: {proximo_evento['professor']})"
        )
    else:
        texto_prova = "📌 **Nenhum evento acadêmico marcado para esta semana!**"

 
 
    return print(texto_prova)

if __name__ == "__main__":
    atualizar_prova_da_semana()