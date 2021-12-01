define-command -docstring "Open a FZF Tmux pane to select file to edit" \
fzf-edit %{ evaluate-commands %sh{
    if [ "$(echo 3.2 $(tmux -V | sed 's/tmux //') | tr ' ' '\n' | sort -V | head -1)" = "3.2" ]; then
        view="-p 80%,50%"
    else
        view="-d 50%"
    fi
    file=$(find . -type f | fzf-tmux $view --preview='bat {}')
    if [ -n "$file" ]; then
        echo "edit '$file'"
    else
        echo "echo no file selected"
    fi
}}
