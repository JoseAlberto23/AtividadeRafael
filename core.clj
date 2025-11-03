(ns integrador.core
  (:require [clojure.string :as str]))

(def alunos (atom []))

(defn ler-string [prompt]
  (print prompt)
  (flush)
  (str/trim (read-line)))

(defn ler-nota [prompt]
  (try
    (print prompt)
    (flush)
    (let [input (read-line)]
      (if (str/blank? input)
        nil
        (Double/parseDouble input)))
    (catch NumberFormatException _
      (println "Entrada inválida. Por favor, digite um número para a nota.")
      (ler-nota prompt))))

(defn calcular-media [notas]
  (if (empty? notas)
    0
    (/ (apply + notas) (count notas))))

(defn status-aluno [nota]
  (if (>= nota 7.0)
    "Aprovado"
    "Reprovado"))


(defn cadastrar-aluno []
  (loop [alunos-temp []]
    (let [nome (ler-string "Nome do aluno (deixe em branco para parar): ")
          aluno-existe (some #(= nome (:nome %)) @alunos)]
      (if (str/blank? nome)
        (do
          (swap! alunos concat alunos-temp)
          (println "\nCadastro finalizado. Alunos adicionados:" (count alunos-temp)))
        (if aluno-existe
          (do
            (println "Aluno com este nome já cadastrado. Por favor, use um nome único.")
            (recur alunos-temp))
          (let [nota (ler-nota (str "Nota de " nome ": "))]
            (if (nil? nota)
              (do
                (println "Nota não pode ser vazia. Tente novamente.")
                (recur alunos-temp))
              (let [novo-aluno {:nome nome :nota nota}]
                (recur (conj alunos-temp novo-aluno))))))))))



(defn relatorio-de-notas []
  (if (empty? @alunos)
    (println "Nenhum aluno cadastrado para gerar o relatório.")
    (let [alunos-com-status (map #(assoc % :status (status-aluno (:nota %))) @alunos)
          aprovados (filter #(= "Aprovado" (:status %)) alunos-com-status)
          notas (map :nota @alunos)
          media-geral (calcular-media notas)]
      
      (println "\n=== Relatório de Notas ===")
      (println "Lista de todos os alunos cadastrados (com status):")
      (doseq [aluno alunos-com-status]
        (println (str "Nome: " (:nome aluno) ", Nota: " (:nota aluno) ", Status: " (:status aluno))))
      
      (println "\nAlunos Aprovados:")
      (if (empty? aprovados)
        (println "Nenhum aluno aprovado.")
        (doseq [aluno aprovados]
          (println (str "Nome: " (:nome aluno) ", Nota: " (:nota aluno)))))
      
      (println (format "\nMédia Geral da Turma: %.2f" media-geral)))))



(defn estatisticas-gerais []
  (if (empty? @alunos)
    (println "Nenhum aluno cadastrado para gerar estatísticas.")
    (let [alunos-com-status (map #(assoc % :status (status-aluno (:nota %))) @alunos)
          total-alunos (count @alunos)
          aprovados (count (filter #(= "Aprovado" (:status %)) alunos-com-status))
          reprovados (- total-alunos aprovados)
          notas (map :nota @alunos)
          media-geral (calcular-media notas)
          maior-nota (apply max notas)
          menor-nota (apply min notas)]
      
      (println "\n=== Estatísticas Gerais ===")
      (println "Total de Alunos Cadastrados:" total-alunos)
      (println "Número de Aprovados:" aprovados)
      (println "Número de Reprovados:" reprovados)
      (println (format "Maior Nota: %.2f" maior-nota))
      (println (format "Menor Nota: %.2f" menor-nota))
      (println (format "Média Geral da Turma: %.2f" media-geral)))))


(defn buscar-aluno []
  (if (empty? @alunos)
    (println "Nenhum aluno cadastrado para realizar a busca.")
    (let [nome-busca (ler-string "Digite o nome do aluno para buscar: ")
          aluno-encontrado (first (filter #(= nome-busca (:nome %)) @alunos))]
      (if aluno-encontrado
        (let [status (status-aluno (:nota aluno-encontrado))]
          (println "\nAluno Encontrado:")
          (println (str "Nome: " (:nome aluno-encontrado)))
          (println (str "Nota: " (:nota aluno-encontrado)))
          (println (str "Status: " status)))
        (println (str "Aluno '" nome-busca "' não encontrado."))))))



(defn exibir-menu []
  (println "\n=== MENU PRINCIPAL ===")
  (println "1 - Cadastrar Alunos")
  (println "2 - Relatório de Notas")
  (println "3 - Estatísticas Gerais")
  (println "4 - Buscar Aluno (Extra)")
  (println "0 - Sair"))

(defn processar-opcao [opcao]
  (case opcao
    "1" (cadastrar-aluno)
    "2" (relatorio-de-notas)
    "3" (estatisticas-gerais)
    "4" (buscar-aluno)
    "0" (println "Saindo do sistema. Até logo!")
    (println "Opção inválida. Por favor, escolha uma opção de 0 a 4.")))

(defn menu-principal []
  (loop []
    (exibir-menu)
    (let [opcao (ler-string "Escolha uma opção: ")]
      (processar-opcao opcao)
      (when (not= opcao "0")
        (recur)))))

(defn -main
  "Função principal que inicia o programa."
  [& args]
  (println "Bem-vindo ao Sistema de Gerenciamento de Alunos em Clojure!")
  (menu-principal))

(comment

  (reset! alunos [])
  (cadastrar-aluno)
  (relatorio-de-notas)
  (estatisticas-gerais)
  (buscar-aluno)
  (menu-principal)
  )
