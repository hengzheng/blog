---
title: shell脚本常用记录
date: 2017-10-11 11:49:15
tags: linux shell
---
### #!/bin/bash
指定解释此脚本的解释器路径。

### 注释
"#"和行尾之间的内容为注释，会被解释器忽略。
``` bash
################################
# @doc
# @usage
# @author
# @data
################################
```

### echo/printf(可以格式化，类似c语言)
标准输出打印。

### echo颜色输出（加-e选项，解释转义）
\033[0m 关闭所有属性
\033[1m 设置高亮度
\033[4m 下划线
\033[5m 闪烁
\033[7m 反显
\033[8m 消隐
\033[30m -- \033[37m 设置前景色 (黑、红、绿、黄、蓝、紫、天蓝、白)
\033[40m -- \033[47m 设置背景色
\033[nA 光标上移n行
\033[nB 光标下移n行
\033[nC 光标右移n行
\033[nD 光标左移n行
\033[y;x 设置光标位置
\033[2J 清屏
\033[K 清除从光标到行尾的内容
\033[s 保存光标位置
\033[u 恢复光标位置
\033[?25l 隐藏光标
\033[?25h 显示光标

### read
标准输入输入。

<!--- more --->

### $N (N为0-9), ${M} (M>9)
脚本或函数传入的第N|M个参数，但是$0只表示脚本名字。

### $#, $\*, $@
$#脚本传入的参数个数（不算脚本本身）。
$\*,$@脚本传入的所有参数，但不包含脚本本身（即$0）。$\*获取的参数不含双引号，参数之间根据系统环境变量$IFS（默认为空格，Tab，Newline）设定的第一个符号(即空格)分隔。$@获取的参数带双引号，参数之间空格分隔。

### $?
最近一个执行语句的退出状态码。

### ${varname}
获取变量varname的值。
${varname:-word}：如果变量存在且非null，返回变量值；否则返回word值，不产生变量。
${varname:=word}：如果变量存在且非null，返回变量值；否则设置varname为word，并返回值，产生变量。
${varname:?msg}：如果变量存在且非null，返回变量值；否则打印varname:msg，并终止脚本。msg可选。
${varname:+word}：如果变量存在且非null，返回word值；否则返回null。
${varname:offset:length}：返回从offset(从0开始)开始，长度为length的子串。length忽略，则返回从offset开始到结尾的子串。offset为负数，则从结尾偏移。varname可以是@，获取连续的某几个参数。
${varname#pattern}：从varname的头部开始匹配pattern，删除最短匹配，并返回剩下的。
${varname##pattern}：从varname的头部开始匹配pattern，删除最长匹配，并返回剩下的。
${varname%pattern}：从varname的尾部开始匹配pattern，删除最短匹配，并返回剩下的。
${varname%%pattern}：从varname的尾部开始匹配pattern，删除最长匹配，并返回剩下的。
${varname/pattern/string}：用string替换varname中第一个匹配pattern的最长匹配。
${varname//pattern/string}：用string替换varname中所有匹配pattern的最长匹配。
${ \#varname}：变量varname值的字符长度。

### $(cmd), \`cmd\`
返回cmd命令执行的输出信息。

### function
函数定义关键字。
``` bash
function F{
    statements
}
或者
function F(){
    statements
}
```
### return
函数使用，可以指定函数返回值。默认是最后一条执行语句的退出状态码。

### if
条件控制语法，elif,else可选。
``` bash
if condition
then
    statements
[elif condition
    then statements...]
[else
 statements]
fi
```

### for
for循环语法。
``` bash
执行M-N次Statements
for x := N to M do
begin
    statements...
end
或者
in list忽略时，表示in "$@"
for name [in list]
do
    statements that can use  $name...
done
算数运算循环
for (( initialisation ; ending condition ; update ))
do
    statements...
done

```

### case
类似switch语法。
``` bash
case expression in
    pattern1  )
        statements ;;
    pattern2  )
        statements ;;
    ...
esac
```

### select
简单的菜单选择器语法。
``` bash
select name [in list]
do
    statements that can use  $name...
done
```

### while/until
while循环语法。while时condition为true，则执行statements；until是condition为false才执行。
``` bash
while/until condition
do
    statements...
done
```
### break
跳出循环。

### test/[]
条件测试，返回退出状态码。注意：[后要接个空格，]前也需要。

### shift N
传入脚本的参数位置左移N个，默认N=1。

### getopts
获取传入脚本带-的选项信息。":ab:c"第一个:冒号表示如果传入了除-a，-b，-c外的选项(被转化成-?)，不打印报错信息（cmd: illegal option -- x）。第二个:冒号表示-b选项带参数。OPTARG变量表示选项的参数，OPTIND变量表示getopts操作参数的索引。
``` bash
while getopts ":ab:c" opt; do
    case $opt in
        a  ) process option -a ;;
        b  ) process option -b
             $OPTARG is the option's argument ;;
        c  ) process option -c ;;
        \? ) echo 'usage: alice [-a] [-b barg] [-c] args...'
             exit 1
    esac
done
```

### $((...))
算数表达式。

### 命令执行优先级
alise > keyword > functions > builtin > scripts or exe-programs
