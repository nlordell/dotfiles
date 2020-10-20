define-command -docstring "Open a FZF Tmux pane to select file to edit" \
fzf-edit %{ evaluate-commands %sh{
    file=$(find . -type f | fzf-tmux -d 30 --preview='bat {}')
    if [ -n "$file" ]; then
        echo "edit '$file'"
    else
        echo "echo no file selected"
    fi
}}
