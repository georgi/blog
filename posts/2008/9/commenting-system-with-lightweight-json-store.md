--- 
category: Javascript
guid: 4ed200b0-5efa-012b-f5dc-001a92975b89
date: 2008-09-07
tags: php, javascript, database, json

Commenting system with lightweight JSON store
=============================================

As I wrote this [blog engine][1], the need for a commenting system
arose and I reflected about a small and simple commenting system with
just a flat file JSON store. This is my solution, which can be used on
any static page on a server with PHP support.

### JQuery Frontend

First of all we need a form, which will post the data to the *PHP*
script. The comment rendering is done by my own *Javascript*
templating system, which I will explain in a seperate post. For now we
focus on the task of posting a form and saving it to our *JSON* store.

    <div class="comment-form">
        <form>
          <input type="hidden" name="guid" value="7ad04f10-5dd6-012b-b53c-001a92975b89"/>
          <p>
            <label>Name</label><br/>
            <input type="text" name="name" size="40"/>
          </p>
          <p>
            <label>Website</label><br/>
            <input type="text" name="website"  size="40"/>
          </p>
          <p>
            <label>Comment</label><br/>
            <textarea name="text" cols="60" rows="10"></textarea>
          </p>
          <p>
            <input type="submit" value="Post comment"/>
          </p>
        </form>
    </div>

My blog engine generates a guid for each post, so this will be posted
by the form as well. Ok, let's have a look at the Javascript code:

    $('.comment-form form').ajaxForm({
        url: Blog.root + 'controllers/comments.php',
        type: 'POST',
        resetForm: true,
        beforeSubmit: function(values) {
            if (values[1].value && values[3].value) {
                return true;
            }
            else {
                alert('Please enter name and text!');
                return false;
            }
        },
        success: function(data) {
            renderComments(data);
        }
    });

This uses the [jquery-form plugin][2] to submit the form via *AJAX*,
nothing special here, the input will be validated to have at least a
name and comment text. After a successful comment post, the comments
should be rendered, but this is another story.

### PHP Backend

The backend is extremely [YAGNI][3]. Comments for one post, will be
saved in one JSON file like `comments/guid-of-the-post`. The 4 fields
will be encoded as json array and appended to the file. This happens
only, if the request was a *POST*. Finally we read the whole file and
send it back as response.

    <?php
    
    $guid_pattern = "/^(\{{0,1}([0-9a-fA-F]){8}-([0-9a-fA-F]){4}-([0-9a-fA-F]){4}-([0-9a-fA-F]){4}-([0-9a-fA-F]){12}\}{0,1})$/";
    $req = $_REQUEST;
    $guid = $req['guid'];
     
    preg_match($guid_pattern, $guid) or die("invalid guid");
     
    $file = 'comments/' . $guid;
    
    if ($_SERVER['REQUEST_METHOD'] == 'POST') {   
      // create a comment record
      $record = array(date('Y-m-d H:i:s'), 
                      strip_tags(stripslashes($req['name'])),
                      strip_tags(stripslashes($req['website'])),
                      strip_tags(stripslashes($req['text'])));
    
      // encode as json string
      $json = json_encode($record) . "\n";
    
      // open the comment file for appending
      $fp = fopen($file, "a");
    
      // acquire a write lock
      flock($fp, LOCK_EX);
    
      // append the json line
      fwrite($fp, $json);
    
      // release lock
      flock($fp, LOCK_UN);
    
      // close file
      fclose($fp);
    }
    
    if (file_exists($file)) {    
      // open the comment file for reading
      $fp = fopen($file, "r");
    
      // acquire a read lock
      flock($fp, LOCK_SH);
    
      // read whole file and print it out
      echo fread($fp, filesize($file));
    
      // release lock
      flock($fp, LOCK_UN);
    
      // close file
      fclose($fp);
    }
    
    ?>

One important thing to note is, that the comment file is not one big
JSON array. It looks like this:

    ["2008-09-07 12:28:33","Hans","","**strong text**\n*emphasized text*"]
    ["2008-09-07 12:29:33","Hans","","**strong text**\n\n\n*emphasized text*"]
    ["2008-09-07 12:29:56","Hans","","**strong text**\n\n\n*emphasized text*"]

For each line, we have one JSON array. This way, the *PHP* script
doesn't need to read the whole JSON thing into memory. It just appends
on every *POST* one line.

### Escaping

Some blog comments showed strange escaping behviour, so I investigated
further. PHP has a foolproof feature called [Magic Quotes][4]. It
automatically escapes all dangerous characters from request parameters
to protect dumb users from [SQL Injection][5]. This feature is
deprecated and in version 6.0.0 it will be removed. Nevertheless it is 
activated in a default PHP installation.

To revert the escaping behaviour I have to call `stripslashes`. Also I
have to care about stripping HTML tags from input. So to protect from
malicious HTML, I filter all input through `strip_tags`.

### Concurrency

As a commenter pointed out, concurrent access can be a headache. I
hoped, that `file_put_contents` is an atomic function, but it is
not. However, I use a simple file locking scheme, which is good
enough. One caveat remains: in a multithreading environment this will
not work reliably. But I think, most PHP installations run as CGI, so
this will be ok.

### Security

Seems that I had a serious security flaw in my first version. I didn't
check the guid parameter, so that you could pass a path like 
`../../../../../../etc/group`. Now the guid is matched against a regular
expression, so the script is now safe.

### Conclusion

With a few lines you can hook up a simple commenting system for static
pages powered by *AJAX* and *PHP*. Note, that rendering of comments is
not discussed here and happens on *Javascript* side. I will write
about my *Javascript template engine* later in a seperate post.




[1]: shinmun-a-small-and-beautiful-blog-engine.html
[2]: http://malsup.com/jquery/form/
[3]: http://en.wikipedia.org/wiki/You_Ain%27t_Gonna_Need_It
[4]: http://de.php.net/manual/en/security.magicquotes.php
[5]: http://en.wikipedia.org/wiki/SQL_injection
